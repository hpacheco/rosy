{-# LANGUAGE ViewPatterns #-}

module Rosy.Viewer.Core where

import Rosy.Robot.State
import Rosy.Robot.Kobuki
import Rosy.Viewer.State

import Ros.Geometry_msgs.Vector3 as Vector3
import Ros.Geometry_msgs.Twist as Twist

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Window (Window(..),Dimension(..))
import qualified Graphics.Gloss.Window as W

import Control.Concurrent.STM

import Lens.Family (over,set)

runViewer :: WorldState -> IO ()
runViewer w = do
    playIO (_worldDisplay w) (greyN 0.5) 60 w drawIO eventIO timeIO

drawIO :: WorldState -> IO Picture
drawIO w = do
    let wdw1 = map (map drawCell) (_worldMap w)
    wdw2 <- drawBotIO w
    wdw3 <- drawMenuIO w
    return $ W.hR (const 200) (W.many [W.vhsSquare wdw1,wdw2]) wdw3 (_worldDimension w)

floorColor = greyN 0.4
wallColor  = makeColor 0.5 0.25 0.25 1
holeColor  = makeColor 0 0.25 0.5 1
robotColor = greyN 0.2
bumperOffColor = black
bumperOnColor = red
menuColor = greyN 0.2

drawCell :: Cell -> Window
drawCell Floor = Color floorColor . W.rectangleWire
drawCell Wall = Color wallColor . W.rectangleSolid
drawCell Hole = Color holeColor . W.rectangleSolid

-- | Converts a coordinate from cm to pixels
scalePx :: WorldState -> Float -> Dimension -> Float
scalePx w cm (dx,dy) = px
    where
    (mx,my) = mapSize $ _worldMap w
    cellPx = min (realToFrac dx / realToFrac mx) (realToFrac dy / realToFrac my)
    px = (cellPx * cm) / realToFrac mapCellSize

drawMenuIO :: WorldState -> IO Window
drawMenuIO w = return $ Color menuColor . W.rectangleSolid

drawBotIO :: WorldState -> IO Window
drawBotIO w = do
    let mkBumperL = do
            isOn <- atomically $ readTVar (_robotEventState $ _robotBumperL $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate 15 $ Color c $ thickArc 60 90 r (r/10)
    let mkBumperC = do
            isOn <- atomically $ readTVar (_robotEventState $ _robotBumperC $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate 15 $ Color c $ thickArc 0 30 r (r/10)
    let mkBumperR = do
            isOn <- atomically $ readTVar (_robotEventState $ _robotBumperC $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate (-15) $ Color c $ thickArc (-60) (-90) r (r/10)
    bl <- mkBumperL
    bc <- mkBumperC
    br <- mkBumperR
    let mkCliffL = do
            isOn <- atomically $ readTVar (_robotEventState $ _robotCliffL $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate (-60) $ Translate (r*3/4) 0 $ Color c $ circleSolid (r/7)
    let mkCliffC = do
            isOn <- atomically $ readTVar (_robotEventState $ _robotCliffC $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Translate (r*3/4) 0 $ Color c $ circleSolid (r/7)
    let mkCliffR = do
            isOn <- atomically $ readTVar (_robotEventState $ _robotCliffR $ _worldRobot w)
            let c = if isOn then bumperOnColor else bumperOffColor
            return $ \r -> Rotate (60) $ Translate (r*3/4) 0 $ Color c $ circleSolid (r/7)
    cl <- mkCliffL
    cc <- mkCliffC
    cr <- mkCliffR
    let metal = Color robotColor . circleSolid
    let mkRobot r = [metal r,bl r,bc r, br r,cl r,cc r,cr r]
    let robot = Pictures . mkRobot . scalePx w (realToFrac robotRadius)
    ang <- _robotOrientation $ _worldRobot w
    return $ W.rotate (realToFrac (-ang)) robot

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

-- Values taken from the kobuki_keyop
changeVel :: SpecialKey -> WorldState -> IO WorldState
changeVel k w = atomically $ modifyTVar (_robotVel $ _worldRobot w) chg >> return w
    where
    chg = case k of
        KeyUp    -> over (Twist.linear  . Vector3.x) (\x -> x+0.05)
        KeyDown  -> over (Twist.linear  . Vector3.x) (\x -> x-0.05)
        KeyLeft  -> over (Twist.angular . Vector3.z) (\x -> x+0.33)
        KeyRight -> over (Twist.angular . Vector3.z) (\x -> x-0.33)

timeIO :: Float -> WorldState -> IO WorldState
timeIO t w = return w

    

