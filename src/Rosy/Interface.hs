{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module Rosy.Interface where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad

import Data.Typeable
import Data.Map (Map(..))
import qualified Data.Map as Map

import Graphics.Gloss

import Ros.Node
import Rosy.Robot.State
import Rosy.Viewer.State
import Rosy.Controller.Core
import Rosy.Robot.Kobuki
import Rosy.Viewer.Core

import Ros.Topic.Util as Topic

import Unsafe.Coerce
import System.IO.Unsafe

startNode :: Node () -> WorldState -> IO ()
startNode n w = runNode "rosy-simulator" $ do
    n
    runRobotNodes w
    runViewerNodes w

-- | The main function that produces a Rosy program.
-- It receives a robot 'Controller' that does the actual job of interacting with your robot.
simulate :: Controller a => a -> IO ()
simulate n = do
    w <- newWorldState world1
    concurrently_ (startNode (controller n) w) (runViewer w)

-- | The main function that produces a Rosy program.
-- It receives a robot 'Controller' that does the actual job of interacting with your robot.
-- It also receives a world in which the robot navigates.
simulateIn :: Controller a => a -> World -> IO ()
simulateIn n w = do
    w <- newWorldState w
    concurrently_ (startNode (controller n) w) (runViewer w)