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
    threads <- runRobot w
    shutdown <- getShutdownAction
    liftIO $ forM_ threads killThread
    liftIO $ shutdown

simulate :: Node () -> WorldState -> IO ()
simulate n w = concurrently_ (startNode n w) (runViewer w)