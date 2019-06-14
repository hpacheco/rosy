{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Rosy.Controller.Kobuki where

import Data.Word as Word
import Data.Typeable
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Lens.Family.TH (makeLensesBy)
import Lens.Family (view, set)

import Rosy.Controller.Core

import Ros.Node
import Ros.Topic as Topic
import Ros.Geometry_msgs.Point as Point
import Ros.Geometry_msgs.Pose as Pose
import Ros.Geometry_msgs.Twist as Twist
import Ros.Geometry_msgs.PoseWithCovariance as PoseWithCovariance
import Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
import Ros.Geometry_msgs.Vector3 as Vector3
import Ros.Geometry_msgs.Quaternion as Quaternion
import Ros.Nav_msgs.Odometry as Odometry
import Ros.Kobuki_msgs.ButtonEvent as ButtonEvent
import Ros.Kobuki_msgs.BumperEvent as BumperEvent
import Ros.Kobuki_msgs.CliffEvent as CliffEvent
import Ros.Kobuki_msgs.WheelDropEvent as WheelDropEvent
import qualified Ros.Kobuki_msgs.Sound as Sound
import qualified Ros.Kobuki_msgs.Led as Led

-- * Kobuki Subscriptions (give orders to the robot)

-- ** Sounds

-- | One of the seven pre-configured robot sounds.
data Sound
    = OnSound
    | OffSound
    | Rechargesound
    | ButtonSound
    | ErrorSound
    | CleaningStartSound
    | CleaningEndSound
  deriving (Show, Eq, Ord, Typeable, G.Generic,Enum)

soundToROS :: Sound -> Sound.Sound
soundToROS = Sound.Sound . toEnum . fromEnum

instance Published Sound where
    published t = advertise "/mobile-base/commands/sound" (fmap soundToROS t)

-- ** Leds

-- | All the possible robot led colors.
data Color = Black | Green | Orange | Red
  deriving (Show, Eq, Ord, Typeable, G.Generic,Enum)
 
instance D.Default Color
 
colorToROS :: Color -> Word.Word8
colorToROS = toEnum . fromEnum

-- | The robot's first led light.
data Led1 = Led1
    { ledColor1 :: Color
    } deriving (Show, Eq, Ord, Typeable, G.Generic)

$(makeLensesBy (Just . (++"Lens")) ''Led1)

led1ToROS :: Led1 -> Led.Led
led1ToROS = Led.Led . colorToROS . ledColor1

instance D.Default Led1

instance Published Led1 where
    published t = advertise "/mobile-base/commands/led1" (fmap led1ToROS t)

-- | The robot's second led light.
data Led2 = Led2
    { ledColor2 :: Color
    } deriving (Show, Eq, Ord, Typeable, G.Generic)

$(makeLensesBy (Just . (++"Lens")) ''Led2)

led2ToROS :: Led2 -> Led.Led
led2ToROS = Led.Led . colorToROS . ledColor2

instance D.Default Led2

instance Published Led2 where
    published t = advertise "/mobile-base/commands/led2" (fmap led2ToROS t)

-- ** Velocity

-- | The velocity of the robot is defined using two parameters.
data Velocity = Velocity
    { -- | Linear velocity in the same direction as the robot (cm/s)
      velocityLinear  :: Double
      -- | Angular velocity in the counter-clockwise direction (radians/s)
    , velocityAngular :: Double
    } deriving (Show, Eq, Ord, Typeable, G.Generic)

$(makeLensesBy (Just . (++"Lens")) ''Velocity)

instance D.Default Velocity

addVelocity :: Velocity -> Velocity -> Velocity
addVelocity (Velocity vx1 az1) (Velocity vx2 az2) = Velocity (vx1+vx2) (az1+az2)

velocityFromROS :: Twist -> Velocity
velocityFromROS t = Velocity (Vector3._x $ Twist._linear t) (Vector3._z $ Twist._angular t)

velocityToROS :: Velocity -> Twist
velocityToROS (Velocity vx az) = Twist (Vector3.Vector3 vx 0 0) (Vector3.Vector3 0 0 az)

instance Published Velocity where
    published t = advertise "/mobile-base/commands/velocity" (fmap velocityToROS t)

-- * Kobuki publications (see the robot's state)

-- ** Odometry

-- | The current position of the robot.
data Position = Position
    { -- | Coordinate in the horizontal X axis.
      positionX :: Double
      -- | Coordinate in the vertical Y axis.
    , positionY :: Double
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Position)

instance D.Default Position

pointToPosition :: Point -> Position
pointToPosition p = Position (Point._x p) (Point._y p)

instance Subscribed Position where
    subscribed = do
        odom <- subscribe "odom"
        return $ fmap (pointToPosition . Pose._position . PoseWithCovariance._pose . Odometry._pose) odom
        
-- | The orientation of the robot.
data Orientation = Orientation
    { -- | Orientation of the robot as an angle relative to the horizontal X axis (radians).
      orientation :: Double
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Orientation)

instance D.Default Orientation

orientationFromROS :: Quaternion -> Orientation
orientationFromROS (Quaternion x y z w) = Orientation $ (atan2 (2*w*z+2*x*y) (1 - 2*(y*y + z*z)))

orientationToROS :: Orientation -> Quaternion
orientationToROS (Orientation yaw) = Quaternion qx qy qz qw
    where
    pitch = 0
    roll = 0
    cy = cos(yaw * 0.5)
    sy = sin(yaw * 0.5)
    cp = cos(pitch * 0.5)
    sp = sin(pitch * 0.5)
    cr = cos(roll * 0.5)
    sr = sin(roll * 0.5)
    qx = cy * cp * sr - sy * sp * cr
    qy = sy * cp * sr + cy * sp * cr
    qz = sy * cp * cr - cy * sp * sr
    qw = cy * cp * cr + sy * sp * sr

instance Subscribed Orientation where
    subscribed = do
        odom <- subscribe "odom"
        return $ fmap (orientationFromROS . Pose._orientation . PoseWithCovariance._pose . Odometry._pose) odom
        
instance Subscribed Velocity where
    subscribed = do
        odom <- subscribe "odom"
        return $ fmap (velocityFromROS . TwistWithCovariance._twist . Odometry._twist) odom
        
-- ** Buttons

-- | When a button is 'Released' or 'Pressed'.
data ButtonStatus = Released | Pressed
    deriving (Show, Eq, Ord, Typeable, G.Generic,Enum)

instance D.Default ButtonStatus

-- | The first button of the robot.
data Button0 = Button0
    { butttonStatus0 :: ButtonStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Button0)

instance D.Default Button0

instance Subscribed Button0 where
    subscribed = do
        buttons <- subscribe "/mobile-base/commands/button"
        let button0 = Topic.filter ((== 0) . ButtonEvent._button) buttons
        return $ fmap (Button0 . toEnum . fromEnum . ButtonEvent._state) button0

-- | The second button of the robot.
data Button1 = Button1
    { buttonStatus1 :: ButtonStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Button1)

instance D.Default Button1

instance Subscribed Button1 where
    subscribed = do
        buttons <- subscribe "/mobile-base/commands/button"
        let button1 = Topic.filter ((== 1) . ButtonEvent._button) buttons
        return $ fmap (Button1 . toEnum . fromEnum . ButtonEvent._state) button1

-- | The third button of the robot.
data Button2 = Button2
    { butttonStatus2 :: ButtonStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Button2)

instance D.Default Button2

instance Subscribed Button2 where
    subscribed = do
        buttons <- subscribe "/mobile-base/commands/button"
        let button2 = Topic.filter ((== 2) . ButtonEvent._button) buttons
        return $ fmap (Button2 . toEnum . fromEnum . ButtonEvent._state) button2

-- ** Bumpers

-- | When a bumper is 'Pressed' against a wall or 'Released' from a wall.
type BumperStatus = ButtonStatus

-- | The left-sided bumper of the robot.
data BumperLeft = BumperLeft
    { bumperStatusLeft :: BumperStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''BumperLeft)

instance D.Default BumperLeft

instance Subscribed BumperLeft where
    subscribed = do
        bumpers <- subscribe "/mobile-base/commands/bumper"
        let bumper = Topic.filter ((== 0) . BumperEvent._bumper) bumpers
        return $ fmap (BumperLeft . toEnum . fromEnum . BumperEvent._state) bumper

-- | The front bumper of the robot.
data BumperCenter = BumperCenter
    { bumperStatusCenter :: BumperStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''BumperCenter)

instance D.Default BumperCenter

instance Subscribed BumperCenter where
    subscribed = do
        bumpers <- subscribe "/mobile-base/commands/bumper"
        let bumper = Topic.filter ((== 1) . BumperEvent._bumper) bumpers
        return $ fmap (BumperCenter . toEnum . fromEnum . BumperEvent._state) bumper

-- | The right-sided bumper of the robot.
data BumperRight = BumperRight
    { bumperRightStatus :: BumperStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''BumperRight)

instance D.Default BumperRight

instance Subscribed BumperRight where
    subscribed = do
        bumpers <- subscribe "/mobile-base/commands/bumper"
        let bumper = Topic.filter ((== 2) . BumperEvent._bumper) bumpers
        return $ fmap (BumperRight . toEnum . fromEnum . BumperEvent._state) bumper

-- ** Cliffs

-- | When a cliff (downward height) sensor is looking at the 'Floor' or at a 'Cliff'.
data CliffStatus = Floor | Cliff
    deriving (Show, Eq, Ord, Typeable, G.Generic,Enum)

instance D.Default CliffStatus

-- | The left-sided cliff sensor of the robot.
data CliffLeft = CliffLeft
    { cliffStatusLeft :: CliffStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''CliffLeft)

instance D.Default CliffLeft

instance Subscribed CliffLeft where
    subscribed = do
        cliffs <- subscribe "/mobile-base/commands/cliff"
        let cliff = Topic.filter ((== 0) . CliffEvent._sensor) cliffs
        return $ fmap (CliffLeft . toEnum . fromEnum . CliffEvent._state) cliff

-- | The front cliff sensor of the robot.
data CliffCenter = CliffCenter
    { cliffStatusCenter :: CliffStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''CliffCenter)

instance D.Default CliffCenter

instance Subscribed CliffCenter where
    subscribed = do
        cliffs <- subscribe "/mobile-base/commands/cliff"
        let cliff = Topic.filter ((== 1) . CliffEvent._sensor) cliffs
        return $ fmap (CliffCenter . toEnum . fromEnum . CliffEvent._state) cliff

-- | The right-sided cliff sensor of the robot.
data CliffRight = CliffRight
    { cliffStatusRight :: CliffStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''CliffRight)

instance D.Default CliffRight

instance Subscribed CliffRight where
    subscribed = do
        cliffs <- subscribe "/mobile-base/commands/cliff"
        let cliff = Topic.filter ((== 2) . CliffEvent._sensor) cliffs
        return $ fmap (CliffRight . toEnum . fromEnum . CliffEvent._state) cliff

-- ** Wheels

-- | When one of the robot's wheels is touching the 'Ground' or is suspended in the 'Air'.
data WheelStatus = Ground | Air
    deriving (Show, Eq, Ord, Typeable, G.Generic,Enum)

instance D.Default WheelStatus

-- | The left-side wheel of the robot.
data WheelLeft = WheelLeft
    { wheelStatusLeft :: WheelStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''WheelLeft)

instance D.Default WheelLeft

instance Subscribed WheelLeft where
    subscribed = do
        wheels <- subscribe "/mobile-base/commands/wheel_drop"
        let wheel = Topic.filter ((== wheel_LEFT) . WheelDropEvent._wheel) wheels
        return $ fmap (WheelLeft . toEnum . fromEnum . WheelDropEvent._state) wheel

-- | The right-side wheel of the robot.
data WheelRight = WheelRight
    { wheelStatusRight :: WheelStatus
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''WheelRight)

instance D.Default WheelRight

instance Subscribed WheelRight where
    subscribed = do
        wheels <- subscribe "/mobile-base/commands/wheel_drop"
        let wheel = Topic.filter ((== wheel_RIGHT) . WheelDropEvent._wheel) wheels
        return $ fmap (WheelRight . toEnum . fromEnum . WheelDropEvent._state) wheel

