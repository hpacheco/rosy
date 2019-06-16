{-# LANGUAGE CPP, TemplateHaskell, DeriveGeneric #-}

module Rosy.Viewer.State where

import Rosy.Robot.State
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
import Ros.Geometry_msgs.Pose as Pose
import qualified Ros.Geometry_msgs.Point as Point

import Control.Concurrent.STM
import Data.Typeable
import Data.Default.Generics as D
import GHC.Generics as G
import GHC.Conc

import Graphics.Gloss
import Graphics.Gloss.Window
import Graphics.Gloss.Interface.Environment

import Lens.Family.TH
import Lens.Family (over,set)

type World = [[Cell]]
data Cell = Grnd | Wall | Hole
    deriving (Show,Typeable, G.Generic,Eq)

data WorldState = WorldState
    { _worldDisplay :: Display -- initial gloss display static
    , _worldDimension :: Dimension -- dimension of the screen in pixels, may be resized
    , _worldMap :: WorldMap -- static
    , _worldRobot :: RobotState -- static
    , _worldVel :: EventState Controller.Velocity -- keyop for desired velocity
    } deriving (Typeable, G.Generic)
    
$(makeLenses ''WorldState)
    
newDisplay :: IO Display
#if defined(ghcjs_HOST_OS)
newDisplay = getDisplay
#else
newDisplay = return $ InWindow "rosy-simulator" (800,800) (0,0)
#endif
    
newWorldState :: World -> IO WorldState
newWorldState wmap = do
    display <- newDisplay
    robotInit <- newRobotState
    dimension <- displayDimension display
    vel <- newEventState D.def
    return $ WorldState display dimension wmap robotInit vel
    
world1 :: World
world1 = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
         ,[Wall,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall,Wall]
         ,[Wall,Grnd,Hole,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall,Wall]
         ,[Wall,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall,Wall]
         ,[Wall,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall]
         ,[Wall,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall]
         ,[Wall,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall]
         ,[Wall,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall]
         ,[Wall,Grnd,Hole,Hole,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall]
         ,[Wall,Grnd,Hole,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall]
         ,[Wall,Grnd,Grnd,Grnd,Grnd,Grnd,Grnd,Wall,Wall,Grnd,Grnd,Wall]
         ,[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]

-- | Size of each map cell in cm.
mapCellSize :: Double
mapCellSize = 30

mapSize :: MapState -> Dimension
mapSize [] = (0,0)
mapSize (l:ls) = (length l,succ $ length ls)

