{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

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
import Data.Default.Generics as D
import GHC.Generics as G
import GHC.Conc

import Graphics.Gloss
import Graphics.Gloss.Window

import Lens.Family.TH
import Lens.Family (over,set)

type MapState = [[Cell]]
data Cell = Floor | Wall | Hole

data WorldState = WorldState
    { _worldDisplay :: Display -- initial gloss display static
    , _worldDimension :: Dimension -- dimension of the screen in pixels, may be resized
    , _worldMap :: MapState -- static
    , _worldRobotPosition :: Pose -- position of the robot in cm, using the gloss referential (center of screen (0,0))
    , _worldRobot :: RobotState -- static
    } deriving (Typeable, G.Generic)
    
$(makeLenses ''WorldState)
    
newWorldState :: IO WorldState
newWorldState = do
    let dimension = (800,800)
    let display = InWindow "rosy-simulator" dimension (0,0)
    robotInit <- newRobotState
    return $ WorldState display dimension mapInit D.def robotInit
    
mapInit :: MapState
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

-- | Size of each map cell in cm.
mapCellSize :: Double
mapCellSize = 30

mapSize :: MapState -> Dimension
mapSize [] = (0,0)
mapSize (l:ls) = (length l,succ $ length ls)

