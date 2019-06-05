{-# LANGUAGE DeriveGeneric #-}

module Rosy.Robot.Kobuki where

import Rosy.Robot.State
import Rosy.Viewer.State

import Ros.Node
import Ros.Rate
import Ros.Topic as Topic hiding (fst,snd)
import Ros.Topic.Util as Topic 
import Ros.Kobuki_msgs.Led as Led
import Ros.Kobuki_msgs.Sound as Sound
import Ros.Kobuki_msgs.BumperEvent as BumperEvent
import Ros.Kobuki_msgs.ButtonEvent as ButtonEvent
import Ros.Kobuki_msgs.CliffEvent as CliffEvent
import Ros.Nav_msgs.Odometry as Odometry

import Control.Concurrent.STM
import Data.Typeable
import Data.Word as Word
import GHC.Generics as G
import GHC.Conc
import System.Process

import Paths_rosy
    
-- ** Robot inputs

playSound :: Sound -> IO ()
playSound i = do
    soundPath <- getDataFileName $ soundCodeToFile (Sound._value i)
    callProcess "play" [soundPath]
  where
    soundCodeToFile 0 = "on.wav"
    soundCodeToFile 1 = "off.wav"
    soundCodeToFile 2 = "recharge.wav"
    soundCodeToFile 3 = "button.wav"
    soundCodeToFile 4 = "error.wav"
    soundCodeToFile 5 = "cleaningstart.wav"
    soundCodeToFile 6 = "cleanindend.wav"

robotSound :: RobotState -> Node ThreadId
robotSound st = do
    sound <- subscribe "/mobile-base/commands/sound"
    flip runHandler sound $ \soundcode -> playSound (soundcode)

robotLed1 :: RobotState -> Node ThreadId
robotLed1 st = do
    led <- subscribe "/mobile-base/commands/led1"
    flip runHandler led $ \ledcolor -> atomically $ writeTVar (_led1 st) ledcolor

robotLed2 :: RobotState -> Node ThreadId
robotLed2 st = do
    led <- subscribe "/mobile-base/commands/led2"
    flip runHandler led $ \ledcolor -> atomically $ writeTVar (_led2 st) ledcolor

robotVelocity :: RobotState -> Node ThreadId
robotVelocity st = do
    v <- subscribe "/mobile-base/commands/velocity"
    flip runHandler v $ \twist -> atomically $ writeTVar (_vel st) twist

-- ** Physics Engine

robotPhysics :: WorldState -> Node ThreadId
robotPhysics w = liftIO $ do
    let st = _worldRobot w
    go <- rateLimiter robotFrequency $ atomically $ do
        o <- readTVar (_odom st)
        -- TODO: fazer coisas
        -- this, for instance, should set the press/release bumper and cliff MVars whenever they change 0->1 and 1->0
        writeTVar (_odom st) o
    forkIO go

-- ** Robot Outputs
    
robotOdometry :: RobotState -> Node ()
robotOdometry st = do
    advertise "odom" $ topicRate 1 $ repeatM $ atomically $ readTVar (_odom st)

robotButtons :: RobotState -> Node ()
robotButtons st = do
    let robotButtonTrigger0 = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _button0 st)
            return $ ButtonEvent button_Button0 (if b then ButtonEvent.state_PRESSED else ButtonEvent.state_RELEASED)
    let robotButtonTrigger1 = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _button1 st)
            return $ ButtonEvent button_Button1 (if b then ButtonEvent.state_PRESSED else ButtonEvent.state_RELEASED)
    let robotButtonTrigger2 = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _button2 st)
            return $ ButtonEvent button_Button2 (if b then ButtonEvent.state_PRESSED else ButtonEvent.state_RELEASED)
    advertise "/mobile-base/events/button" $ Topic.mergeList
        [robotButtonTrigger0,robotButtonTrigger1,robotButtonTrigger2]

robotBumpers :: RobotState -> Node ()
robotBumpers st = do
    let robotBumperTriggerL = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _bumperL st)
            return $ BumperEvent bumper_LEFT (if b then BumperEvent.state_PRESSED else BumperEvent.state_RELEASED)
    let robotBumperTriggerC = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _bumperC st)
            return $ BumperEvent bumper_CENTER (if b then BumperEvent.state_PRESSED else BumperEvent.state_RELEASED)
    let robotBumperTriggerR = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _bumperR st)
            return $ BumperEvent bumper_RIGHT (if b then BumperEvent.state_PRESSED else BumperEvent.state_RELEASED)
    advertise "/mobile-base/events/bumper" $ Topic.mergeList
        [robotBumperTriggerL,robotBumperTriggerC,robotBumperTriggerR]

robotCliffs :: RobotState -> Node ()
robotCliffs st = do
    let robotCliffTriggerL = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _cliffL st)
            return $ CliffEvent CliffEvent.sensor_LEFT (if b then CliffEvent.state_CLIFF else CliffEvent.state_FLOOR) 1
    let robotCliffTriggerC = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _cliffC st)
            return $ CliffEvent CliffEvent.sensor_CENTER (if b then CliffEvent.state_CLIFF else CliffEvent.state_FLOOR) 1
    let robotCliffTriggerR = repeatM $ atomically $ do
            b <- takeTMVar (_robotEventTrigger $ _cliffR st)
            return $ CliffEvent CliffEvent.sensor_RIGHT (if b then CliffEvent.state_CLIFF else CliffEvent.state_FLOOR) 1
    advertise "/mobile-base/events/cliff" $ Topic.mergeList
        [robotCliffTriggerL,robotCliffTriggerC,robotCliffTriggerR]

runRobot :: WorldState -> Node [ThreadId]
runRobot w = do
    let st = _worldRobot w
    t0 <- robotSound st
    t1 <- robotLed1 st
    t2 <- robotLed2 st
    t3 <- robotVelocity st
    t4 <- robotPhysics w
    robotOdometry st
    robotButtons st
    robotBumpers st
    robotCliffs st
    return [t0,t1,t2,t3,t4]
    
-- | Robot maximum translational velocity cm/s
robotMaxLinearSpeed :: Double
robotMaxLinearSpeed = 70

-- | Robot maximum rotational velocity deg/s
robotMaxRotationalSpeed :: Double
robotMaxRotationalSpeed = 180
    
-- | Robot size in cm.
robotSize :: Double
robotSize = 35.15
robotRadius :: Double
robotRadius = robotSize / 2
    
    
    
    
    