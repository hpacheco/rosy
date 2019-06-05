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
    playIO (_worldDisplay w) (greyN 0.4) 60 w drawIO eventIO timeIO

drawIO :: WorldState -> IO Picture
drawIO w = do
    let wdw1 = map (map drawCell) (_worldMap w)
    wdw2 <- drawBotIO w
    return $ W.many [W.vhs wdw1,wdw2] (_worldDimension w)

floorColor = makeColor 0.8 1 0.8 1
wallColor  = makeColor 0.5 0.25 0.25 1
holeColor  = makeColor 0 0.25 0.5 1
robotColor = greyN 0.8

drawCell :: Cell -> Window
drawCell Floor = Color floorColor . W.rectangleSolid
drawCell Wall = Color wallColor . W.rectangleSolid
drawCell Hole = Color holeColor . W.rectangleSolid

-- | Converts a coordinate from cm to pixels
scalePx :: WorldState -> Float -> Dimension -> Float
scalePx w cm (dx,dy) = px
    where
    (mx,my) = mapSize $ _worldMap w
    cellPx = min (realToFrac dx / realToFrac mx) (realToFrac dy / realToFrac my)
    px = (cellPx * cm) / realToFrac mapCellSize

drawBotIO :: WorldState -> IO Window
drawBotIO w = return $ Color robotColor . circleSolid . scalePx w (realToFrac robotRadius)

eventIO :: Event -> WorldState -> IO WorldState
eventIO (EventKey (Char '0') kst _ _) w = reactButton _button0 kst w
eventIO (EventKey (Char '1') kst _ _) w = reactButton _button1 kst w
eventIO (EventKey (Char '2') kst _ _) w = reactButton _button2 kst w
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
changeVel k w = atomically $ modifyTVar (_vel $ _worldRobot w) chg >> return w
    where
    chg = case k of
        KeyUp    -> over (Twist.linear  . Vector3.x) (\x -> x+0.05)
        KeyDown  -> over (Twist.linear  . Vector3.x) (\x -> x-0.05)
        KeyLeft  -> over (Twist.angular . Vector3.z) (\x -> x+0.33)
        KeyRight -> over (Twist.angular . Vector3.z) (\x -> x-0.33)

timeIO :: Float -> WorldState -> IO WorldState
timeIO t w = return w

    

