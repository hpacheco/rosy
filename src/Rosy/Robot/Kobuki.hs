{-# LANGUAGE DeriveGeneric, ViewPatterns #-}

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

import Paths_rosy
    
-- ** Robot inputs

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

readRobotSound :: RobotState -> Node ThreadId
readRobotSound st = do
    sound <- subscribe "/mobile-base/commands/sound"
    flip runHandler sound $ \soundcode -> playSound (soundcode)

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
        let vlin' = vlin + alin 
        let vrot' = vrot + arot 
        let rads' = rads + vrot' / robotFrequency
        let px' = px + vlin' * cos rads' / robotFrequency
        let py' = py + vlin' * sin rads' / robotFrequency
        
        -- check if robot hit a wall
        --let hitWall = any (==Wall) $ robotCells w (px',py')
        --let (px'',py'') = if hitWall then (px,py) else (px',py')
        
        let chgV twist = set (Twist.linear . Vector3.x) vlin'
                       $ set (Twist.angular . Vector3.z) vrot' twist
        let chgP pose = set (Pose.orientation) (Controller.orientationToROS $ Controller.Orientation rads')
                      $ set (Pose.position . Point.x) px'
                      $ set (Pose.position . Point.y) py' pose
        writeTVar (_robotOdom st) $
            over (Odometry.pose . PoseWithCovariance.pose) chgP $
            over (Odometry.twist . TwistWithCovariance.twist) chgV o

    forkIO $ forever go

sensorPos :: Double -> (Double,Double) -> (Double,Double)
sensorPos rads (px,py) = (px + robotRadius * cos rads,py + robotRadius * sin rads)

posCell :: WorldState -> (Double,Double) -> Maybe Cell
posCell w p = Monad.join $ fmap (flip atMay $ floor pc) (m `atMay` floor pl)
    where
    (pl,pc) = posToMap w p
    m = _worldMap w

posToMap :: WorldState -> (Double,Double) -> (Double,Double)
posToMap w (px,py) = (mx/2 + px/mapCellSize ,my/2 - py/mapCellSize)
    where
    m = _worldMap w
    (realToFrac -> mx,realToFrac -> my) = mapSize m

robotCells :: WorldState -> (Double,Double) -> [Cell]
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
    
-- frequency of the physics engine
robotFrequency :: Double
robotFrequency = 10
    
    
    
    