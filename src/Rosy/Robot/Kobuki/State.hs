{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Rosy.Robot.Kobuki.State where

import qualified Rosy.Controller.Kobuki as Controller

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
import Ros.Geometry_msgs.Twist as Twist
import Ros.Geometry_msgs.PoseWithCovariance as PoseWithCovariance
import Ros.Geometry_msgs.Pose as Pose

import Control.Concurrent.STM
import Control.Monad
import Data.Time.Clock
import Data.Typeable
import Data.Default.Generics as D
import GHC.Generics as G
import GHC.Conc
import Lens.Family.TH
import Lens.Family (over,set)


data EventState a = EventState
    { _eventState   :: TVar a -- internal state
    , _eventTrigger :: TMVar a -- trigger when pressed/released
    } deriving (Typeable, G.Generic)

$(makeLenses ''EventState)

type RobotEventState = EventState Bool

data RobotState = RobotState
    { _robotLed1      :: TVar Led
    , _robotLed2      :: TVar Led
    , _robotVel       :: TVar (Twist,UTCTime) -- desired velocity
    , _robotDrag      :: TVar Double -- drag velocity
    , _robotOdom      :: TVar Odometry
    , _robotButton0   :: RobotEventState 
    , _robotButton1   :: RobotEventState 
    , _robotButton2   :: RobotEventState 
    , _robotBumperL   :: RobotEventState 
    , _robotBumperC   :: RobotEventState 
    , _robotBumperR   :: RobotEventState 
    , _robotCliffL    :: RobotEventState 
    , _robotCliffC    :: RobotEventState 
    , _robotCliffR    :: RobotEventState 
    , _robotWheelL    :: RobotEventState 
    , _robotWheelR    :: RobotEventState 
    } deriving (Typeable, G.Generic)

$(makeLenses ''RobotState)

_robotPose :: RobotState -> IO Pose
_robotPose st = atomically $ do
    o <- readTVar $ _robotOdom st
    return $ (PoseWithCovariance._pose . Odometry._pose) o

newEventState :: a -> IO (EventState a)
newEventState def = atomically $ do
    state <- newTVar def
    trigger <- newEmptyTMVar
    return $ EventState state trigger

newRobotState :: IO RobotState
newRobotState = atomically $ do
    led1 <- newTVar $ D.def
    led2 <- newTVar $ D.def
    
    vel <- newTVar $ D.def
    vdrag <- newTVar $ D.def
    odom <- newTVar $ D.def
    
    buttonState0 <- newTVar False
    buttonTrigger0 <- newEmptyTMVar
    let button0 = EventState buttonState0 buttonTrigger0
    buttonState1 <- newTVar False
    buttonTrigger1 <- newEmptyTMVar
    let button1 = EventState buttonState1 buttonTrigger1
    buttonState2 <- newTVar False
    buttonTrigger2 <- newEmptyTMVar
    let button2 = EventState buttonState2 buttonTrigger2
    
    bumperStateL <- newTVar False
    bumperTriggerL <- newEmptyTMVar
    let bumperL = EventState bumperStateL bumperTriggerL
    bumperStateC <- newTVar False
    bumperTriggerC <- newEmptyTMVar
    let bumperC = EventState bumperStateC bumperTriggerC
    bumperStateR <- newTVar False
    bumperTriggerR <- newEmptyTMVar
    let bumperR = EventState bumperStateR bumperTriggerR
    
    cliffStateL <- newTVar False
    cliffTriggerL <- newEmptyTMVar
    let cliffL = EventState cliffStateL cliffTriggerL
    cliffStateC <- newTVar False
    cliffTriggerC <- newEmptyTMVar
    let cliffC = EventState cliffStateC cliffTriggerC
    cliffStateR <- newTVar False
    cliffTriggerR <- newEmptyTMVar
    let cliffR = EventState cliffStateR cliffTriggerR
    
    wheelStateL <- newTVar False
    wheelTriggerL <- newEmptyTMVar
    let wheelL = EventState wheelStateL wheelTriggerL
    wheelStateR <- newTVar False
    wheelTriggerR <- newEmptyTMVar
    let wheelR = EventState wheelStateR wheelTriggerR
    
    return $ RobotState
        led1 led2
        vel vdrag odom
        button0 button1 button2
        bumperL bumperC bumperR
        cliffL cliffC cliffR
        wheelL wheelR

changeRobotEventState :: RobotEventState -> Bool -> STM ()
changeRobotEventState st isPressedNew = do
    writeTVar (_eventState st) isPressedNew 
    putTMVar (_eventTrigger st) isPressedNew

debug :: String -> IO ()
debug msg = putStrLn msg



