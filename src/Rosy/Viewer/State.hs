{-# LANGUAGE DeriveGeneric #-}

module Rosy.Viewer.State where

import Rosy.Robot.State

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
import Ros.Geometry_msgs.Pose as Pose

import Control.Concurrent.STM
import Data.Typeable
import GHC.Generics as G
import GHC.Conc

import Graphics.Gloss

data WorldState = WorldState
    { worldDisplay :: Display
    , worldMap :: Map
    , worldRobot :: RobotState -- static
    } deriving (Typeable, G.Generic)
    
newWorldState :: Pose -> IO WorldState
newWorldState pose = do
    let display = InWindow "rosy-simulator" (800,800) (0,0)
    robotInit <- newRobotState pose
    return $ WorldState display mapInit robotInit
    
type Map = [[Cell]]
data Cell = Floor | Wall | Hole
    
mapInit :: Map
mapInit = [[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall,Wall]
          ,[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall]
          ,[Floor,Floor,Hole,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall]
          ,[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall]
          ,[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall]
          ,[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor]
          ,[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor]
          ,[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor]
          ,[Floor,Floor,Hole,Hole,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor]
          ,[Floor,Floor,Hole,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor]
          ,[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall,Floor,Floor,Floor]
          ,[Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Floor]]



