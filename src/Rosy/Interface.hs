{-# LANGUAGE ScopedTypeVariables, TupleSections, UndecidableInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Rosy.Interface where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.Hierarchy
import Control.Monad
import Control.Monad.Trans

import Data.Typeable
import Data.Default.Generics
import Data.Map (Map(..))
import qualified Data.Map as Map

import Graphics.Gloss

import Ros.Node
import Rosy.Controller.Core
import Rosy.Robot.Kobuki.State as Kobuki
import Rosy.Viewer.Kobuki.State as Kobuki
import Rosy.Robot.Kobuki.Core as Kobuki
import Rosy.Viewer.Kobuki.Core as Kobuki
import Rosy.Robot.Turtlesim.State as Turtle
import Rosy.Viewer.Turtlesim.State as Turtle
import Rosy.Robot.Turtlesim.Core as Turtle
import Rosy.Viewer.Turtlesim.Core as Turtle

import Ros.Topic.Util as Topic
import Ros.Topic as Topic

import Unsafe.Coerce
import System.IO.Unsafe
import GHC.Conc

control :: Runnable a => a -> IO ()
control n = runNode "rosy-simulator" (run n)

-- | Launches simulated kobuki viewer and robot nodes alongside the controller node
startKobuki :: Node () -> Kobuki.WorldState -> IO ()
startKobuki n w = runNode "rosy-simulator" $ do
    Kobuki.runViewerNodes w
    Kobuki.runRobotNodes w
    n
    
-- | Launches simulated turtlesim viewer and robot nodes alongside the controller node
startTurtle :: Node () -> Turtle.WorldState -> IO ()
startTurtle n w = runNode "rosy-simulator" $ do
    Turtle.runViewerNodes w
    Turtle.runRobotNodes w
    n

-- | The kinds of simulated robots.
data Robot
    -- | A Kobuki robot, simulated in a given world
    = Kobuki { kobukiWorld :: Maybe World }
    -- | A Turtlesim robot
    | Turtlesim

-- | The main function that produces a Rosy program.
-- It receives a 'Runnable' robot controller that does the actual job of interacting with your robot.
-- It also receives the kind of 'Robot' that you want to control.
simulate :: Runnable a => Robot -> a -> IO ()
simulate (Kobuki w) n = simulateKobuki (maybe world1 id w) $ run n
simulate (Turtlesim) n = simulateTurtle $ run n

simulateKobuki :: World -> Node () -> IO ()
simulateKobuki world n = do
    w <- Kobuki.newWorldState world
    concurrently_ (startKobuki n w) (Kobuki.runViewer w)
    
simulateTurtle :: Node () -> IO ()
simulateTurtle n = do
    w <- Turtle.newWorldState
    concurrently_ (startTurtle n w) (Turtle.runViewer w)


    
