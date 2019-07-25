{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Rosy.Viewer.Core where

import Rosy.Robot.State
import Rosy.Robot.Kobuki
import Rosy.Viewer.State
import qualified Rosy.Controller.Kobuki as Controller
import Rosy.Controller.Core
import Rosy.Util

import Ros.Geometry_msgs.Vector3 as Vector3
import Ros.Geometry_msgs.Twist as Twist
import qualified Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
import qualified Ros.Geometry_msgs.Point as Point
import qualified Ros.Geometry_msgs.Pose as Pose
import qualified Ros.Geometry_msgs.PoseWithCovariance as PoseWithCovariance
import Ros.Kobuki_msgs.Led as Led
import Ros.Nav_msgs.Odometry as Odometry
import Ros.Node
import Ros.Topic as Topic
import Ros.Topic.Util as Topic
import Ros.Rate

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Window (Window(..),Dimension(..))
import qualified Graphics.Gloss.Window as W

import Control.Concurrent.STM
import Control.Monad

import Data.Default.Generics as D
import Data.Maybe

import GHC.Conc

import Text.Printf

import Lens.Family (over,set)

import System.FilePath

runViewer :: WorldState -> IO ()
runViewer w = do
    playIO (_worldDisplay w) (greyN 0.5) 30 w drawIO eventIO timeIO

drawIO :: WorldState -> IO Picture
drawIO w = do
    o <- atomically $ readTVar (_robotOdom $ _worldRobot w)
    let wdw1 = map (map drawCell) (_worldMap w)
    wdw2 <- drawBotIO w o
    wdw3 <- drawMenuIO w o
    let wdw = W.many [W.vhsSquare wdw1,wdw2]
    --return $ wdw (_worldDimension w)
    return $ W.many [wdw,wdw3] (_worldDimension w)

groundColor = greyN 0.4 -- medium dark grey
wallColor  = makeColor 0.5 0.25 0.25 1 -- redish
holeColor  = makeColor 0 0.25 0.5 1 -- blueish
robotColor = greyN 0.2 -- dark grey
bumperOffColor = black
bumperOnColor = red
menuColor = greyN 0.2 -- dark grey

drawCell :: Cell -> Window
drawCell Grnd = Color groundColor . W.rectangleWire
drawCell Wall = Color wallColor . W.rectangleSolid
drawCell Hole = Color holeColor . W.rectangleSolid

-- | Converts a coordinate from m to pixels
scalePx :: WorldState -> Float -> Dimension -> Float
scalePx w m (dx,dy) = px
    where
    (mx,my) = mapSize $ _worldMap w
    cellPx = min (realToFrac dx / realToFrac mx) (realToFrac dy / realToFrac my)
    px = (cellPx * m) / realToFrac mapCellSize

scalePointPx :: WorldState -> (Float,Float) -> Dimension -> (Float,Float)
scalePointPx w (cmx,cmy) d = (scalePx w cmx d,scalePx w cmy d)

drawMenuIO :: WorldState -> Odometry -> IO Window
drawMenuIO w o = do
    
    -- get status
    let vlin = Vector3._x $ Twist._linear $ TwistWithCovariance._twist $ Odometry._twist o
    let vrot = Vector3._z $ Twist._angular $ TwistWithCovariance._twist $ Odometry._twist o
    let pos = Pose._position $ PoseWithCovariance._pose $ Odometry._pose o
    let px = Point._x pos
    let py = Point._y pos
    let Controller.Orientation rads = Controller.orientationFromROS $ Pose._orientation $ PoseWithCovariance._pose $ Odometry._pose o
    
    -- draw menus
    let back = Color menuColor . W.rectangleSolid
    let positionx   = Translate (-70) 0 . Scale 0.13 0.13 . W.text ("PositionX: " ++ printf "%.2f" px ++ " m")
    let positiony   = Translate (-70) 0 . Scale 0.13 0.13 . W.text ("PositionY: " ++ printf "%.2f" py ++ " m")
    let orientation = Translate (-70) 0 . Scale 0.13 0.13 . W.text ("Orientation: " ++ printf "%.2f" rads ++ " rads")
    let lvelocity   = Translate (-70) 0 . Scale 0.13 0.13 . W.text ("Linear Velocity: " ++ printf "%.2f" vlin ++" m/s")
    let avelocity   = Translate (-70) 0 . Scale 0.13 0.13 . W.text ("Angular Velocity: " ++ printf "%.2f" vrot ++" rads/s")
    let info1 = W.hs [positionx,positiony,orientation]
    let info2 = W.hs [lvelocity,avelocity]
    
    return $ W.vT (const 50) info1 (W.vB (const 50) W.empty info2)

ledColor :: Led -> Color
ledColor l = case Led._value l of
    0 -> black
    1 -> green
    2 -> orange
    3 -> red
    
drawBotIO :: WorldState -> Odometry -> IO Window
drawBotIO w o = do
    let mkLed1 = do
            c <- liftM ledColor $ atomically $ readTVar (_robotLed1 $ _worldRobot w)
            return $ \r -> Translate (-r*1/3) (r*3/4) $ Color c $ circleSolid (r/10)
    let mkLed2 = do
            c <- liftM ledColor $ atomically $ readTVar (_robotLed2 $ _worldRobot w)
            return $ \r -> Translate (-r*1/3) (r*2/4) $ Color c $ circleSolid (r/10)
    l1 <- mkLed1
    l2 <- mkLed2
    let mkButton0 = do
            isOn <- atomically $ readTVar (_eventState . _robotButton0 $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Translate (-r*1/3) (-r*1/4) $ Color c $ rectangleSolid (r/5) (r/5)
    let mkButton1 = do
            isOn <- atomically $ readTVar (_eventState . _robotButton1 $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Translate (-r*1/3) (-r*2/4) $ Color c $ rectangleSolid (r/5) (r/5)
    let mkButton2 = do
            isOn <- atomically $ readTVar (_eventState . _robotButton2 $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Translate (-r*1/3) (-r*3/4) $ Color c $ rectangleSolid (r/5) (r/5)
    bu0 <- mkButton0
    bu1 <- mkButton1
    bu2 <- mkButton2
    let mkBumperL = do
            isOn <- atomically $ readTVar (_eventState $ _robotBumperL $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate 15 $ Color c $ thickArc 60 90 r (r/10)
    let mkBumperC = do
            isOn <- atomically $ readTVar (_eventState $ _robotBumperC $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate 15 $ Color c $ thickArc 0 30 r (r/10)
    let mkBumperR = do
            isOn <- atomically $ readTVar (_eventState $ _robotBumperR $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate (-15) $ Color c $ thickArc (-60) (-90) r (r/10)
    bl <- mkBumperL
    bc <- mkBumperC
    br <- mkBumperR
    let mkCliffL = do
            isOn <- atomically $ readTVar (_eventState $ _robotCliffL $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate (-60) $ Translate (r*3/4) 0 $ Color c $ circleSolid (r/7)
    let mkCliffC = do
            isOn <- atomically $ readTVar (_eventState $ _robotCliffC $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Translate (r*3/4) 0 $ Color c $ circleSolid (r/7)
    let mkCliffR = do
            isOn <- atomically $ readTVar (_eventState $ _robotCliffR $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate (60) $ Translate (r*3/4) 0 $ Color c $ circleSolid (r/7)
    cl <- mkCliffL
    cc <- mkCliffC
    cr <- mkCliffR
    let mkWheelL = do
            isOn <- atomically $ readTVar (_eventState $ _robotWheelL $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Translate 0 (r/3) $ Color c $ rectangleSolid (r*2/5) (r*1/5)
    let mkWheelR = do
            isOn <- atomically $ readTVar (_eventState $ _robotWheelR $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Translate 0 (-r/3) $ Color c $ rectangleSolid (r*2/5) (r*1/5)
    wh1 <- mkWheelL
    wh2 <- mkWheelR
    let metal = Color robotColor . circleSolid
    let mkRobot r = [metal r,bl r,bc r, br r,cl r,cc r,cr r,l1 r, l2 r,bu0 r,bu1 r,bu2 r,wh1 r,wh2 r]
    let robot = Pictures . mkRobot . scalePx w (realToFrac robotRadius)
    let pose = PoseWithCovariance._pose $ Odometry._pose o
    let ang = radiansToDegrees $ realToFrac $ Controller.orientation $ Controller.orientationFromROS $ Pose._orientation pose
    let posx = realToFrac $ Point._x $ Pose._position pose
    let posy = realToFrac $ Point._y $ Pose._position pose
    return $ \dim -> Translate (scalePx w posx dim) (scalePx w posy dim) $ Rotate (-ang) $ robot dim

eventIO :: Event -> WorldState -> IO WorldState
eventIO (EventKey (Char '0') kst _ _) w = reactButton _robotButton0 kst w
eventIO (EventKey (Char '1') kst _ _) w = reactButton _robotButton1 kst w
eventIO (EventKey (Char '2') kst _ _) w = reactButton _robotButton2 kst w
eventIO (EventKey (SpecialKey k@(isArrowKey -> True)) Down _ _) w = changeVel k w
eventIO (EventResize d) w = return $ set worldDimension d w
eventIO e w = return w

isArrowKey :: SpecialKey -> Bool
isArrowKey KeyUp = True
isArrowKey KeyDown = True
isArrowKey KeyLeft = True
isArrowKey KeyRight = True
isArrowKey _ = False

keyStateToBool :: KeyState -> Bool
keyStateToBool Down = True
keyStateToBool Up = False

reactButton :: (RobotState -> RobotEventState) -> KeyState -> WorldState -> IO WorldState
reactButton getButton kst w = atomically $ changeRobotEventState (getButton $ _worldRobot w) (keyStateToBool kst) >> return w

-- Values taken from the kobuki_keyop: 0.05, 0.33
changeVel :: SpecialKey -> WorldState -> IO WorldState
changeVel k w = atomically $ do
    modifyTVar (_worldVel w) chg
    return w
  where
    chg = case k of
        KeyUp    -> over (Controller.velocityLinearLens) (\x -> x+0.5)   
        KeyDown  -> over (Controller.velocityLinearLens) (\x -> x-0.5)   
        KeyLeft  -> over (Controller.velocityAngularLens) (\x -> x+0.33) 
        KeyRight -> over (Controller.velocityAngularLens) (\x -> x-0.33) 
        _        -> id

timeIO :: Float -> WorldState -> IO WorldState
timeIO t w = return w

writeViewerVelocity :: WorldState -> Node ()
writeViewerVelocity w = do
    go <- liftIO $ rateLimiter keyOpFrequency $ atomically $ do
        v <- readTVar (_worldVel w)
        if (v==D.def)
            then return Nothing
            else return $ Just $ Controller.velocityToROS v
    let viewerVelocityTrigger = fmap fromJust $ Topic.filter isJust $ Topic.repeatM (liftTIO go)
    advertise (roshome </> "commands/velocity") $ viewerVelocityTrigger
 where
    liftTIO :: IO a -> TIO a
    liftTIO = liftIO
    
runViewerNodes :: WorldState -> Node ()
runViewerNodes w = do
    writeViewerVelocity w

keyOpFrequency :: Double
keyOpFrequency = 10

