{-# LANGUAGE TupleSections, CPP, DeriveGeneric, ScopedTypeVariables, ViewPatterns #-}

module Rosy.Robot.Kobuki where

import Rosy.Robot.State
import Rosy.Viewer.State
import qualified Rosy.Controller.Kobuki as Controller
import Rosy.Util

import Ros.Node
import Ros.Rate
import Ros.Topic as Topic hiding (fst,snd,forever)
import Ros.Topic.Util as Topic 
import Ros.Kobuki_msgs.Led as Led
import Ros.Kobuki_msgs.Sound as Sound
import Ros.Kobuki_msgs.BumperEvent as BumperEvent
import Ros.Kobuki_msgs.ButtonEvent as ButtonEvent
import Ros.Kobuki_msgs.CliffEvent as CliffEvent
import Ros.Kobuki_msgs.WheelDropEvent as WheelDropEvent
import Ros.Nav_msgs.Odometry as Odometry
import Ros.Geometry_msgs.Twist as Twist
import Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
import Ros.Geometry_msgs.Pose as Pose
import Ros.Geometry_msgs.PoseWithCovariance as PoseWithCovariance
import Ros.Geometry_msgs.Vector3 as Vector3
import Ros.Geometry_msgs.Point as Point

import Control.Concurrent.STM
import Control.Monad as Monad
import Data.Typeable
import Data.Word as Word
import GHC.Generics as G
import GHC.Conc
import System.Process
import Safe
import Prelude as P
import Data.Maybe as Maybe

import Lens.Family (over,set)

import System.FilePath

import Graphics.Gloss.Interface.Environment

import Control.Exception

import Paths_rosy

-- ** Robot inputs

orNothing :: IO () -> IO ()
orNothing m = catch m (\(e::SomeException) -> return ())

#if defined(ghcjs_HOST_OS)
playSound :: Sound -> IO ()
playSound i = do
    playAudioById $ "sound" ++ show (Sound._value i)
#else
playSound :: Sound -> IO ()
playSound i = do
    soundPath <- getDataFileName $ "sounds" </> soundCodeToFile (Sound._value i)
    callProcess "play" [soundPath]
#endif

soundCodeToFile 0 = "on.wav"
soundCodeToFile 1 = "off.wav"
soundCodeToFile 2 = "recharge.wav"
soundCodeToFile 3 = "button.wav"
soundCodeToFile 4 = "error.wav"
soundCodeToFile 5 = "cleaningstart.wav"
soundCodeToFile 6 = "cleanindend.wav"

-- the sound may play before we load the viewer, issuing a js error
readRobotSound :: RobotState -> Node ThreadId
readRobotSound st = do
    sound <- subscribe "/mobile-base/commands/sound"
    flip runHandler sound $ \soundcode -> orNothing $ playSound (soundcode)

readRobotLed1 :: RobotState -> Node ThreadId
readRobotLed1 st = do
    led <- subscribe "/mobile-base/commands/led1"
    flip runHandler led $ \ledcolor -> do
        atomically $ writeTVar (_robotLed1 st) ledcolor

readRobotLed2 :: RobotState -> Node ThreadId
readRobotLed2 st = do
    led <- subscribe "/mobile-base/commands/led2"
    flip runHandler led $ \ledcolor -> atomically $ writeTVar (_robotLed2 st) ledcolor

readRobotVelocity :: RobotState -> Node ThreadId
readRobotVelocity st = do
    v <- subscribe "/mobile-base/commands/velocity"
    flip runHandler v $ \twist -> atomically $ writeTVar (_robotVel st) twist

-- ** Physics Engine

runRobotPhysics :: WorldState -> Node ThreadId
runRobotPhysics w = liftIO $ do
    let st = _worldRobot w
    go <- rateLimiter robotFrequency $ atomically $ do
        -- original robot position + velocity
        o <- readTVar (_robotOdom st)
        vdrag <- readTVar (_robotDrag st)
        let vlin = Vector3._x $ Twist._linear $ TwistWithCovariance._twist $ Odometry._twist o
        let vrot = Vector3._z $ Twist._angular $ TwistWithCovariance._twist $ Odometry._twist o
        let pos = Pose._position $ PoseWithCovariance._pose $ Odometry._pose o
        let px = Point._x pos
        let py = Point._y pos
        let Controller.Orientation rads = Controller.orientationFromROS $ Pose._orientation $ PoseWithCovariance._pose $ Odometry._pose o
        
        -- acceleration towards desired velocity
        vel'' <- readTVar (_robotVel st)
        let vlin'' = Vector3._x $ Twist._linear vel''
        let vrot'' = Vector3._z $ Twist._angular vel''
        let alin = min robotMaxLinearAccel (vlin'' - vlin)
        let arot = min robotMaxRotationalAccel (vrot'' - vrot)
        
        -- new robot position + velocity (computes linear and angular velocities separately)
        let v = scalarVec vlin rads `addVec` scalarVec vdrag (rads+pi/2)
        let vrot' = vrot + arot 
        let rads' = rads + vrot' / robotFrequency
        let v0 = v `addVec` scalarVec alin rads'
        let vlin' = magnitudeVec v0 * cos (angleVec v0 - rads')
        let vdrag0 = magnitudeVec v0 * sin (angleVec v0 - rads')
        let vdrag' = if vdrag0 > 0
                then max (vdrag0 - robotWheelDragFriction) 0
                else min (vdrag0 + robotWheelDragFriction) 0
        let v' = scalarVec vlin' rads' `addVec` scalarVec vdrag' (rads'+pi/2)
        let (px',py') = (px,py) `addVec` (v' `divVec` robotFrequency)
        
        let changeRobotEvent bmp newbool = do
                oldbool <- readTVar (_eventState $ bmp st)
                writeTVar (_eventState $ bmp st) newbool
                when (oldbool /= newbool) $ putTMVar (_eventTrigger $ bmp st) newbool
        
        let bumper b bmp = changeRobotEvent bmp $ any (==Wall) $ sensorCells w (rads'+b) (px',py')
        bumper (degreesToRadians 60)    _robotBumperL
        bumper (degreesToRadians 0)     _robotBumperC
        bumper (degreesToRadians (-60)) _robotBumperR
        
        let wheel isLeft bmp = changeRobotEvent bmp $ any (==Hole) $ wheelCells w isLeft rads' (px',py')
        wheel True    _robotWheelL
        wheel False   _robotWheelR
        isWheelL <- readTVar (_eventState $ _robotWheelL st)
        isWheelR <- readTVar (_eventState $ _robotWheelR st)
        
        let chgV vx vz twist = set (Twist.linear . Vector3.x) vx
                       $ set (Twist.angular . Vector3.z) vz twist
        let chgP (px,py) rads pose = set (Pose.orientation) (Controller.orientationToROS $ Controller.Orientation rads)
                      $ set (Pose.position . Point.x) px
                      $ set (Pose.position . Point.y) py pose
        let chgVP vx vd vz p rads = do
                writeTVar (_robotDrag st) vd
                writeTVar (_robotOdom st) $
                    over (Odometry.pose . PoseWithCovariance.pose) (chgP p rads) $
                    over (Odometry.twist . TwistWithCovariance.twist) (chgV vx vz) o
        
        -- physics of collision reference: https://www.myphysicslab.com/engine2D/collision-en.html
        -- walls have infinite mass
        if (isWheelL || isWheelR)
            then do -- if at least least one wheel in the air, don't move at all
                chgVP 0 0 0 (px,py) rads
            else  case findRobotCollision w (px',py') of
                Nothing -> do
                    chgVP vlin' vdrag' vrot' (px',py') rads'
                Just (cp,cn) -> do
                    -- elasticity of collision
                    let e = 0.5
                    -- linear velocity before collision
                    let va1 = scalarVec vlin' rads' `addVec` scalarVec vdrag' (rads'+pi/2)
                    -- angular velocity before collision
                    let wa1 = vrot'
                    -- distance vector from center of mass of robot to point of collision
                    -- a bit of an hack, we use:
                    -- . the original robot position (as if it did not move)
                    -- . the collision point (that may be outside the area of the robot!)
                    let rap = cp `subVec` (px,py)
                    -- initial pre-collision velocity of collision point of the robot
                    let vap1 = va1 `addVec` (wa1 `mulVec` rap)
                    let j = (- (1 + e) * dotProdVec vap1 cn) / (1/robotMass + (crossProdVec rap cn)^2 / robotInertia)
                    -- linear velocity after collision
                    let va2 = va1 `addVec` ((j `mulVec` cn) `divVec` robotMass)
                    let wa2 = wa1 + (rap `crossProdVec` (j `mulVec` cn)) / robotInertia
                    -- new linear velocity (consider only new velocity in the axis of the wheels)
                    let vlin2 = (magnitudeVec va2) * cos (angleVec va2-rads')
                    let vdrag2 = (magnitudeVec va2) * sin (angleVec va2-rads')
                    -- use old point before collision (as if it did not move)
                    chgVP vlin2 vdrag2 wa2 (px,py) rads'

    forkIO $ forever go

-- returns (averages if multiple collisions)
-- point of collision with a wall 
-- normal (perpendicular) vector to the wall at the point of collision
findRobotCollision :: WorldState -> DPoint -> Maybe (DPoint,DVector)
findRobotCollision w pXY@(pX,pY) = case collisions of
    [] -> Nothing
    (unzip -> (ps,ns)) -> Just (averageVec ps,normVec $ angleVec $ sumVec ns)
  where
    m = _worldMap w
    minX = pX - robotRadius
    maxX = pX + robotRadius
    minY = pY - robotRadius
    maxY = pY + robotRadius 
    (minC::Int) = floor $ posXToMapC w minX
    (maxC::Int) = ceiling $ posXToMapC w maxX
    (minL::Int) = floor $ posYToMapL w maxY
    (maxL::Int) = ceiling $ posYToMapL w minY
    pLC = posToMap w pXY
    pointsL = concatMap (circleLineIntersection (pLC,robotRadius / mapCellSize)) lins
    pointsC = concatMap (circleLineIntersection (pLC,robotRadius / mapCellSize)) cols
    
    pointsL' = intersectMapLins w pointsL
    pointsC' = intersectMapCols w pointsC
    collisions = map (mapToPos w >< (swap . negVec)) pointsL' ++ map (mapToPos w >< swap) pointsC'
    
    mkLin l = ((l,0),(l,1))
    mkCol c = ((0,c),(1,c))
    lins = map mkLin $ map realToFrac [minL..maxL]
    cols = map mkCol $ map realToFrac [minC..maxC]

intersectMapLins :: WorldState -> [DPoint] -> [(DPoint,DVector)]
intersectMapLins w [] = []
intersectMapLins w (p:ps) = intersectMapLin w p ++ intersectMapLins w ps

intersectMapLin :: WorldState -> DPoint -> [(DPoint,DVector)]
intersectMapLin w (round -> l,c) = cell1++cell2
    where
    cell1 = case mapCell w (realToFrac $ pred l,c) of
        Just Wall -> [((realToFrac l,c),(1,0))]
        otherwise -> []
    cell2 = case mapCell w (realToFrac l,c) of
        Just Wall -> [((realToFrac l,c),(-1,0))]
        otherwise -> []

intersectMapCols :: WorldState -> [DPoint] -> [(DPoint,DVector)]
intersectMapCols w [] = []
intersectMapCols w (p:ps) = intersectMapCol w p ++ intersectMapCols w ps

intersectMapCol :: WorldState -> DPoint -> [(DPoint,DVector)]
intersectMapCol w (l,round -> c) = cell1++cell2
    where
    cell1 = case mapCell w (l,realToFrac $ pred c) of
        Just Wall -> [((l,realToFrac c),(0,1))]
        otherwise -> []
    cell2 = case mapCell w (l,realToFrac c) of
        Just Wall -> [((l,realToFrac c),(0,-1))]
        otherwise -> []

wheelCells :: WorldState -> Bool -> Double -> DPoint -> [Cell]
wheelCells w isLeft rads p = Maybe.maybeToList $ (posCell w) p'
    where
    op = if isLeft then (+) else (-)
    p' = p `addVec` scalarVec (robotRadius/6) (rads `op` pi/2)

sensorCells :: WorldState -> Double -> DPoint -> [Cell]
sensorCells w rads p = Maybe.catMaybes $ map (posCell w) [sensorPos (rads-degreesToRadians 15) p,sensorPos rads p,sensorPos (rads+degreesToRadians 15) p]

-- consider only one point for a sensor
sensorPos :: Double -> DPoint -> DPoint
sensorPos rads (px,py) = (px + robotRadius * cos rads,py + robotRadius * sin rads)

-- find the map cell for a position in the world
posCell :: WorldState -> DPoint -> Maybe Cell
posCell w p = mapCell w (posToMap w p)

-- find the map cell for a position in the map
mapCell :: WorldState -> DPoint -> Maybe Cell
mapCell w (pl,pc) = Monad.join $ fmap (flip atMay $ floor pc) (m `atMay` floor pl)
    where
    m = _worldMap w

-- converts a world position to a map position
posToMap :: WorldState -> DPoint -> DPoint
posToMap w (px,py) = (ml/2 - py/mapCellSize,mc/2 + px/mapCellSize)
    where
    m = _worldMap w
    (realToFrac -> ml,realToFrac -> mc) = mapSize m

posXToMapC,posYToMapL :: WorldState -> Double -> Double
posXToMapC w = snd . posToMap w . (,0)
posYToMapL w = fst . posToMap w . (0,)

-- converts a map position to a world position
mapToPos :: WorldState -> DPoint -> DPoint
mapToPos w (pl,pc) = ((pc - mc/2) * mapCellSize,(-pl + ml/2) * mapCellSize)
    where
    m = _worldMap w
    (realToFrac -> ml,realToFrac -> mc) = mapSize m

robotCells :: WorldState -> DPoint -> [Cell]
robotCells w p = Maybe.catMaybes $ map (posCell w) ps
    where
    angles = [0,30..360]
    ps = map (flip sensorPos p) angles

-- ** Robot Outputs
    
writeRobotOdometry :: RobotState -> Node ()
writeRobotOdometry st = do
    advertise "odom" $ topicRate 1 $ repeatM $ atomically $ readTVar (_robotOdom st)

writeRobotButtons :: RobotState -> Node ()
writeRobotButtons st = do
    let robotButtonTrigger0 = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotButton0 st)
            return $ ButtonEvent button_Button0 (if b then ButtonEvent.state_PRESSED else ButtonEvent.state_RELEASED)
    let robotButtonTrigger1 = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotButton1 st)
            return $ ButtonEvent button_Button1 (if b then ButtonEvent.state_PRESSED else ButtonEvent.state_RELEASED)
    let robotButtonTrigger2 = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotButton2 st)
            return $ ButtonEvent button_Button2 (if b then ButtonEvent.state_PRESSED else ButtonEvent.state_RELEASED)
    advertise "/mobile-base/events/button" $ Topic.mergeList
        [robotButtonTrigger0,robotButtonTrigger1,robotButtonTrigger2]

writeRobotBumpers :: RobotState -> Node ()
writeRobotBumpers st = do
    let robotBumperTriggerL = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotBumperL st)
            return $ BumperEvent bumper_LEFT (if b then BumperEvent.state_PRESSED else BumperEvent.state_RELEASED)
    let robotBumperTriggerC = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotBumperC st)
            return $ BumperEvent bumper_CENTER (if b then BumperEvent.state_PRESSED else BumperEvent.state_RELEASED)
    let robotBumperTriggerR = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotBumperR st)
            return $ BumperEvent bumper_RIGHT (if b then BumperEvent.state_PRESSED else BumperEvent.state_RELEASED)
    advertise "/mobile-base/events/bumper" $ Topic.mergeList
        [robotBumperTriggerL,robotBumperTriggerC,robotBumperTriggerR]

writeRobotCliffs :: RobotState -> Node ()
writeRobotCliffs st = do
    let robotCliffTriggerL = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotCliffL st)
            return $ CliffEvent CliffEvent.sensor_LEFT (if b then CliffEvent.state_CLIFF else CliffEvent.state_FLOOR) 1
    let robotCliffTriggerC = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotCliffC st)
            return $ CliffEvent CliffEvent.sensor_CENTER (if b then CliffEvent.state_CLIFF else CliffEvent.state_FLOOR) 1
    let robotCliffTriggerR = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotCliffR st)
            return $ CliffEvent CliffEvent.sensor_RIGHT (if b then CliffEvent.state_CLIFF else CliffEvent.state_FLOOR) 1
    advertise "/mobile-base/events/cliff" $ Topic.mergeList
        [robotCliffTriggerL,robotCliffTriggerC,robotCliffTriggerR]

writeRobotWheels :: RobotState -> Node ()
writeRobotWheels st = do
    let robotWheelTriggerL = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotWheelL st)
            return $ WheelDropEvent WheelDropEvent.wheel_LEFT (if b then WheelDropEvent.state_DROPPED else WheelDropEvent.state_RAISED)
    let robotWheelTriggerR = repeatM $ atomically $ do
            b <- takeTMVar (_eventTrigger $ _robotWheelR st)
            return $ WheelDropEvent WheelDropEvent.wheel_RIGHT (if b then WheelDropEvent.state_DROPPED else WheelDropEvent.state_RAISED)
    advertise "/mobile-base/events/wheel_drop" $ Topic.mergeList
        [robotWheelTriggerL,robotWheelTriggerR]

runRobotNodes :: WorldState -> Node [ThreadId]
runRobotNodes w = do
    let st = _worldRobot w
    t0 <- readRobotSound st
    t1 <- readRobotLed1 st
    t2 <- readRobotLed2 st
    t3 <- readRobotVelocity st
    t4 <- runRobotPhysics w
    writeRobotOdometry st
    writeRobotButtons st
    writeRobotBumpers st
    writeRobotCliffs st
    writeRobotWheels st
    return [t0,t1,t2,t3,t4]
    
-- cm/s2
robotMaxLinearAccel :: Double
robotMaxLinearAccel = 20

-- radians/s2
robotMaxRotationalAccel :: Double
robotMaxRotationalAccel = 1.35
    
-- | Robot maximum translational velocity cm/s
robotMaxLinearSpeed :: Double
robotMaxLinearSpeed = 70

-- | Robot maximum rotational velocity radians/s
robotMaxRotationalSpeed :: Double
robotMaxRotationalSpeed = pi
    
-- | Robot size in cm.
robotSize :: Double
robotSize = 35.15
robotRadius :: Double
robotRadius = robotSize / 2
    
-- frequency of the physics engine (Hz)
robotFrequency :: Double
robotFrequency = 10

-- moment of inertia of robot
robotInertia :: Double
robotInertia = pi * (robotRadius^4) / 4
    
-- mass of the robot (kg)
robotMass :: Double
robotMass = 2.35
    
-- friction factor [0..1] of the wheels
-- negative velocity perpendicular to the wheels (cm/s)
robotWheelDragFriction :: Double
robotWheelDragFriction = 1