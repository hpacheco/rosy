{-# LANGUAGE CPP, TemplateHaskell, DeriveGeneric #-}

module Rosy.Viewer.Turtlesim.State where

import Rosy.Robot.Turtlesim.State
import qualified Rosy.Controller.Core as Controller
import qualified Rosy.Controller.Turtlesim as Controller

import Ros.Node
import Ros.Rate
import Ros.Topic as Topic hiding (fst,snd)
import Ros.Topic.Util as Topic 
--import Ros.Kobuki_msgs.Led as Led
--import Ros.Kobuki_msgs.Sound as Sound
--import Ros.Kobuki_msgs.BumperEvent as BumperEvent
--import Ros.Kobuki_msgs.ButtonEvent as ButtonEvent
--import Ros.Kobuki_msgs.CliffEvent as CliffEvent
--import Ros.Nav_msgs.Odometry as Odometry
--import Ros.Geometry_msgs.Pose as Pose
--import qualified Ros.Geometry_msgs.Point as Point

import Control.Concurrent.STM
import Data.Time.Clock
import Data.Typeable
import Data.Default.Generics as D
import GHC.Generics as G
import GHC.Conc

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Window
import Graphics.Gloss.Interface.Environment

import Lens.Family.TH
import Lens.Family (over,set)

data WorldState = WorldState
    { _worldStateDisplay :: Display -- initial gloss display static
    , _worldStateDimension :: Dimension -- dimension of the screen in pixels, may be resized
    , _worldStateBackground :: TVar Color -- static
    , _worldStateRobots :: [RobotState] -- static
    } deriving (Typeable, G.Generic)
    
$(makeLenses ''WorldState)
    
newDisplay :: IO Display
#if defined(ghcjs_HOST_OS)
newDisplay = getDisplay
#else
newDisplay = return $ InWindow "rosy-simulator" (500,500) (0,0)
#endif
    
newWorldState :: IO WorldState
newWorldState = do
    display <- newDisplay
    robotInits <- newRobotStates
    back <- newTVarIO backgroundColor
    dimension <- displayDimension display
    return $ WorldState display dimension back robotInits



