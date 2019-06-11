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
import qualified Ros.Kobuki_msgs.Sound as Sound
import qualified Ros.Kobuki_msgs.Led as Led

-- * Kobuki Subscriptions (give orders to the robot)

-- ** Sounds

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
    published t = advertise "/mobile_base/commands/sound" (fmap soundToROS t)

-- ** Leds

data Color = Black | Green | Orange | Red
  deriving (Show, Eq, Ord, Typeable, G.Generic,Enum)
 
instance D.Default Color
 
colorToROS :: Color -> Word.Word8
colorToROS = toEnum . fromEnum

data Led1 = Led1
    { ledColor1 :: Color
    } deriving (Show, Eq, Ord, Typeable, G.Generic)

$(makeLensesBy (Just . (++"Lens")) ''Led1)

led1ToROS :: Led1 -> Led.Led
led1ToROS = Led.Led . colorToROS . ledColor1

instance D.Default Led1

instance Published Led1 where
    published t = advertise "/mobile_base/commands/led1" (fmap led1ToROS t)

data Led2 = Led2
    { ledColor2 :: Color
    } deriving (Show, Eq, Ord, Typeable, G.Generic)

$(makeLensesBy (Just . (++"Lens")) ''Led2)

led2ToROS :: Led2 -> Led.Led
led2ToROS = Led.Led . colorToROS . ledColor2

instance D.Default Led2

instance Published Led2 where
    published t = advertise "/mobile_base/commands/led2" (fmap led2ToROS t)

-- ** Velocity

data Velocity = Velocity
    { velX :: Double -- linear velocity in the X axis, in cm/s
    , angZ :: Double -- angular velocity in the Z axis, in radians/s
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
    published t = advertise "/mobile_base/commands/velocity" (fmap velocityToROS t)

-- * Kobuki publications (see the robot's state)

-- ** Odometry

data Position = Position
    { posX :: Double
    , posY :: Double
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Position)

instance D.Default Position

pointToPosition :: Point -> Position
pointToPosition p = Position (Point._x p) (Point._y p)

instance Subscribed Position where
    subscribed = do
        odom <- subscribe "odom"
        return $ fmap (pointToPosition . Pose._position . PoseWithCovariance._pose . Odometry._pose) odom
        
data Orientation = Orientation
    { rotZ :: Double -- in radians
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
        
data Button0 = Button0
    { -- | 'True' for pressed, 'False' for released
      but0 :: Bool
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Button0)

instance D.Default Button0

instance Subscribed Button0 where
    subscribed = do
        buttons <- subscribe "/mobile_base/commands/button"
        let button0 = Topic.filter ((== 0) . ButtonEvent._button) buttons
        return $ fmap (Button0 . (>0) . ButtonEvent._state) button0

data Button1 = Button1
    { -- | 'True' for pressed, 'False' for released
      but1 :: Bool
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Button1)

instance D.Default Button1

instance Subscribed Button1 where
    subscribed = do
        buttons <- subscribe "/mobile_base/commands/button"
        let button1 = Topic.filter ((== 1) . ButtonEvent._button) buttons
        return $ fmap (Button1 . (>0) . ButtonEvent._state) button1

data Button2 = Button2
    { -- | 'True' for pressed, 'False' for released
      but2 :: Bool
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Button2)

instance D.Default Button2

instance Subscribed Button2 where
    subscribed = do
        buttons <- subscribe "/mobile_base/commands/button"
        let button2 = Topic.filter ((== 2) . ButtonEvent._button) buttons
        return $ fmap (Button2 . (>0) . ButtonEvent._state) button2

-- ** Bumpers

data BumperLeft = BumperLeft
    { -- | 'True' for pressed, 'False' for released
      bumpLeft :: Bool
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''BumperLeft)

instance D.Default BumperLeft

instance Subscribed BumperLeft where
    subscribed = do
        bumpers <- subscribe "/mobile_base/commands/bumper"
        let bumper = Topic.filter ((== 0) . BumperEvent._bumper) bumpers
        return $ fmap (BumperLeft . (>0) . BumperEvent._state) bumper

data BumperCenter = BumperCenter
    { -- | 'True' for pressed, 'False' for released
      bumpCenter :: Bool
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''BumperCenter)

instance D.Default BumperCenter

instance Subscribed BumperCenter where
    subscribed = do
        bumpers <- subscribe "/mobile_base/commands/bumper"
        let bumper = Topic.filter ((== 1) . BumperEvent._bumper) bumpers
        return $ fmap (BumperCenter . (>0) . BumperEvent._state) bumper

data BumperRight = BumperRight
    { bumpRight :: Bool -- | 'True' for pressed, 'False' for released
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''BumperRight)

instance D.Default BumperRight

instance Subscribed BumperRight where
    subscribed = do
        bumpers <- subscribe "/mobile_base/commands/bumper"
        let bumper = Topic.filter ((== 2) . BumperEvent._bumper) bumpers
        return $ fmap (BumperRight . (>0) . BumperEvent._state) bumper

-- ** Cliffs

data CliffLeft = CliffLeft
    { -- | 'True' for approaching, 'False' for moving away
      cliffLeft :: Bool
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''CliffLeft)

instance D.Default CliffLeft

instance Subscribed CliffLeft where
    subscribed = do
        cliffs <- subscribe "/mobile_base/commands/cliff"
        let cliff = Topic.filter ((== 0) . CliffEvent._sensor) cliffs
        return $ fmap (CliffLeft . (>0) . CliffEvent._state) cliff

data CliffCenter = CliffCenter
    { -- | 'True' for approaching, 'False' for moving away
      cliffCenter :: Bool
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''CliffCenter)

instance D.Default CliffCenter

instance Subscribed CliffCenter where
    subscribed = do
        cliffs <- subscribe "/mobile_base/commands/cliff"
        let cliff = Topic.filter ((== 1) . CliffEvent._sensor) cliffs
        return $ fmap (CliffCenter . (>0) . CliffEvent._state) cliff

data CliffRight = CliffRight
    { -- | 'True' for approaching, 'False' for moving away
      cliffRight :: Bool
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''CliffRight)

instance D.Default CliffRight

instance Subscribed CliffRight where
    subscribed = do
        cliffs <- subscribe "/mobile_base/commands/cliff"
        let cliff = Topic.filter ((== 2) . CliffEvent._sensor) cliffs
        return $ fmap (CliffRight . (>0) . CliffEvent._state) cliff

