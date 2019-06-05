{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Rosy.Robot.State where

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
import Data.Typeable
import Data.Default.Generics as D
import GHC.Generics as G
import GHC.Conc
import Lens.Family.TH
import Lens.Family (over,set)

data RobotEventState = RobotEventState
    { _robotEventState   :: TVar Bool -- internal state
    , _robotEventTrigger :: TMVar Bool -- trigger when pressed/released
    } deriving (Typeable, G.Generic)

$(makeLenses ''RobotEventState)

-- frequency of the physics engine
robotFrequency :: Double
robotFrequency = 60

data RobotState = RobotState
    { _led1      :: TVar Led
    , _led2      :: TVar Led
    , _vel       :: TVar Twist -- desired velocity
    , _odom      :: TVar Odometry
    , _button0   :: RobotEventState
    , _button1   :: RobotEventState
    , _button2   :: RobotEventState
    , _bumperL   :: RobotEventState
    , _bumperC   :: RobotEventState
    , _bumperR   :: RobotEventState
    , _cliffL    :: RobotEventState
    , _cliffC    :: RobotEventState
    , _cliffR    :: RobotEventState
    } deriving (Typeable, G.Generic)

$(makeLenses ''RobotState)

newRobotState :: IO RobotState
newRobotState = atomically $ do
    led1 <- newTVar $ D.def
    led2 <- newTVar $ D.def
    
    vel <- newTVar $ D.def
    odom <- newTVar $ D.def
    
    buttonState0 <- newTVar False
    buttonTrigger0 <- newEmptyTMVar
    let button0 = RobotEventState buttonState0 buttonTrigger0
    buttonState1 <- newTVar False
    buttonTrigger1 <- newEmptyTMVar
    let button1 = RobotEventState buttonState1 buttonTrigger1
    buttonState2 <- newTVar False
    buttonTrigger2 <- newEmptyTMVar
    let button2 = RobotEventState buttonState2 buttonTrigger2
    
    bumperStateL <- newTVar False
    bumperTriggerL <- newEmptyTMVar
    let bumperL = RobotEventState bumperStateL bumperTriggerL
    bumperStateC <- newTVar False
    bumperTriggerC <- newEmptyTMVar
    let bumperC = RobotEventState bumperStateC bumperTriggerC
    bumperStateR <- newTVar False
    bumperTriggerR <- newEmptyTMVar
    let bumperR = RobotEventState bumperStateR bumperTriggerR
    
    cliffStateL <- newTVar False
    cliffTriggerL <- newEmptyTMVar
    let cliffL = RobotEventState cliffStateL cliffTriggerL
    cliffStateC <- newTVar False
    cliffTriggerC <- newEmptyTMVar
    let cliffC = RobotEventState cliffStateC cliffTriggerC
    cliffStateR <- newTVar False
    cliffTriggerR <- newEmptyTMVar
    let cliffR = RobotEventState cliffStateR cliffTriggerR
    
    return $ RobotState led1 led2 vel odom button0 button1 button2 bumperL bumperC bumperR cliffL cliffC cliffR

changeRobotEventState :: RobotEventState -> Bool -> STM ()
changeRobotEventState st isPressedNew = do
    writeTVar (_robotEventState st) isPressedNew 
    putTMVar (_robotEventTrigger st) isPressedNew




