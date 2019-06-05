{-# LANGUAGE ViewPatterns #-}

module Rosy.Viewer.Core where

import Rosy.Robot.State
import Rosy.Viewer.State

import Ros.Geometry_msgs.Vector3 as Vector3
import Ros.Geometry_msgs.Twist as Twist

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Window as W

import Control.Concurrent.STM

import Lens.Family (over)

runViewer :: WorldState -> IO ()
runViewer w = do
    playIO (worldDisplay w) (greyN 0.5) 60 w drawIO eventIO timeIO

drawIO :: WorldState -> IO Picture
drawIO w = do
    wdw1 <- mapM (mapM drawCellIO) (worldMap w)
    wdw2 <- drawBotIO (worldRobot w)
    d <- W.displayDimension $ worldDisplay w
    return $ W.many [W.vhs wdw1,wdw2] d

drawCellIO :: Cell -> IO Window
drawCellIO = undefined

drawBotIO :: RobotState -> IO Window
drawBotIO = undefined

eventIO :: Event -> WorldState -> IO WorldState
eventIO (EventKey (Char '0') kst _ _) w = reactButton button0 kst w
eventIO (EventKey (Char '1') kst _ _) w = reactButton button1 kst w
eventIO (EventKey (Char '2') kst _ _) w = reactButton button2 kst w
eventIO (EventKey (SpecialKey k@(isArrowKey -> True)) Down _ _) w = changeVel k w
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
reactButton getButton kst w = atomically $ changeRobotEventState (getButton $ worldRobot w) (keyStateToBool kst) >> return w

-- Values taken from the kobuki_keyop
changeVel :: SpecialKey -> WorldState -> IO WorldState
changeVel k w = atomically $ modifyTVar (vel $ worldRobot w) chg >> return w
    where
    chg = case k of
        KeyUp    -> over (Twist.linear  . Vector3.x) (\x -> x+0.05)
        KeyDown  -> over (Twist.linear  . Vector3.x) (\x -> x-0.05)
        KeyLeft  -> over (Twist.angular . Vector3.z) (\x -> x+0.33)
        KeyRight -> over (Twist.angular . Vector3.z) (\x -> x-0.33)

timeIO :: Float -> WorldState -> IO WorldState
timeIO t w = return w
    

