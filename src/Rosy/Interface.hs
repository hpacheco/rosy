module Rosy.Interface where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad

import Graphics.Gloss

import Ros.Node
import Rosy.Robot.State
import Rosy.Viewer.State
import Rosy.Controller.Core
import Rosy.Robot.Kobuki
import Rosy.Viewer.Core

startNode :: Node () -> WorldState -> IO ()
startNode n w = runNode "rosy-simulator" $ do
    n
    runRobotNodes w
    runViewerNodes w

simulate :: Node () -> IO ()
simulate n = do
    w <- newWorldState
    concurrently_ (startNode n w) (runViewer w)