{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Rosy.Viewer.Turtlesim.Core where

import Rosy.Robot.Turtlesim.State
import Rosy.Robot.Turtlesim.Core
import Rosy.Viewer.Turtlesim.State
import qualified Rosy.Controller.Core as Controller
import qualified Rosy.Controller.Turtlesim as Controller
import Rosy.Controller.Core
import Rosy.Util

--import Ros.Geometry_msgs.Vector3 as Vector3
--import Ros.Geometry_msgs.Twist as Twist
--import qualified Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
--import qualified Ros.Geometry_msgs.Point as Point
--import qualified Ros.Geometry_msgs.Pose as Pose
--import qualified Ros.Geometry_msgs.PoseWithCovariance as PoseWithCovariance
--import Ros.Kobuki_msgs.Led as Led
--import Ros.Nav_msgs.Odometry as Odometry
import Ros.Turtlesim.Pose as Pose
import Ros.Node
import Ros.Topic as Topic
import Ros.Topic.Util as Topic
import Ros.Rate

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Window (Window(..),Dimension(..))
import qualified Graphics.Gloss.Window as W

import Control.Concurrent.STM
import Control.Monad

import Data.Default.Generics as D
import Data.Maybe
import Data.DList as DList

import GHC.Conc

import Text.Printf

import Lens.Family (over,set)

import System.FilePath

runViewer :: WorldState -> IO ()
runViewer w = do
    back <- atomically $ readTVar (_worldStateBackground w)
    playIO (_worldStateDisplay w) back 30 w drawIO eventIO timeIO

drawIO :: WorldState -> IO Picture
drawIO w = do
    let dim = _worldStateDimension w
    back <- atomically $ readTVar (_worldStateBackground w)
    let backfill = Color back $ W.rectangleSolid dim 
    robots <- forM (_worldStateRobots w) drawRobot
    let (paths,turtles) = unzip robots
    return $ Pictures $ backfill : paths ++ turtles
    
lastDList :: DList a -> DList a
lastDList Nil = DList.empty
lastDList xs = DList.singleton (last (DList.toList xs))
    
drawRobot :: RobotState -> IO (Picture,Picture)
drawRobot r = do
    pose <- atomically $ readTVar (_robotPose r)
    let posx = Pose._x pose
    let posy = Pose._y pose
    on <- liftM not $ atomically $ isEmptyTMVar (_robotOn r)
    let ang = radiansToDegrees $ Pose._theta pose
    let turtlepic = Translate (scalePx posx) (scalePx posy) $ Rotate (-ang+90) (_robotImage r)
    let turtle = if on then turtlepic else Blank
    paths <- atomically $ do
        (pics,paths) <- readTVar (_robotPath r)
        let pics' = drawLines (DList.toList paths)
        let pics'' = pics ++ pics'
        writeTVar (_robotPath r) (pics'',lastDList paths)
        return pics''
    return (Pictures paths,turtle)

drawLine :: ((Float,Float),Int,Color) -> ((Float,Float),Int,Color) -> Picture
drawLine (p1,w1,c1) (p2,_,_) = Color c1 $ thickLine (realToFrac w1) (scalePointPx p1) (scalePointPx p2)

drawCircle ((x,y),w,c) = Translate (scalePx x) (scalePx y) $ Color c $ circleSolid (realToFrac w/2)

drawLines :: [((Float,Float),Int,Color)] -> [Picture]
drawLines [] = []
drawLines [l] = [drawCircle l]
drawLines (l1:l2:ls) = drawCircle l1 : drawLine l1 l2 : drawLines (l2:ls)

sameColor (a,b,c) (x,y,z) = c == z
    
thickLine :: Float -> (Float,Float) -> (Float,Float) -> Picture
thickLine w p1 p2 = Polygon [addVec p1 (scalarVec w2 (ang+90)),addVec p1 (scalarVec w2 (ang-90)),addVec p2 (scalarVec w2 (ang-90)),addVec p2 (scalarVec w2 (ang+90))]
    where
    ang = angleVec $ subVec p2 p1 
    w2 = w / 2
    
-- | One meter in pixels
meterSize :: Float
meterSize = 45

windowSize :: Float
windowSize = 500

-- | Converts a coordinate from m to pixels
scalePx :: Float -> Float
scalePx m = m * meterSize - windowSize / 2

scalePointPx :: (Float,Float) -> (Float,Float)
scalePointPx (cmx,cmy) = (scalePx cmx,scalePx cmy)

eventIO :: Event -> WorldState -> IO WorldState
eventIO e w = return w

timeIO :: Float -> WorldState -> IO WorldState
timeIO t w = return w
    
runViewerNodes :: WorldState -> Node ()
runViewerNodes w = do
    return ()


