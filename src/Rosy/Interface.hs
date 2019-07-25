{-# LANGUAGE UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Rosy.Interface where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Hierarchy
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

control :: Controller a => a -> IO ()
control n = runNode "rosy-simulator" (runUserNode $ controller n)

startNode :: Node () -> WorldState -> IO ()
startNode n w = runNode "rosy-simulator" $ do
    runViewerNodes w
    runRobotNodes w
    n

-- | The main function that produces a Rosy program.
-- It receives a robot 'Controller' that does the actual job of interacting with your robot.
simulate :: Controller a => a -> IO ()
simulate = simulateIn world1

-- | The main function that produces a Rosy program.
-- It receives a robot 'Controller' that does the actual job of interacting with your robot.
-- It also receives a world in which the robot navigates.
simulateIn :: Controller a => World -> a -> IO ()
simulateIn w n = simulateNode w $ runUserNode $ controller n

simulateNode :: World -> Node () -> IO ()
simulateNode world n = do
    w <- newWorldState world
    concurrently_ (startNode n w) (runViewer w)

-- | A monad for performing multiple robot tasks, returning a final event
newtype Task end = Task { runTask :: Node end }
    deriving (Applicative,Functor,Monad)

until :: (Subscribed end,Controller b) => b -> Task end
until go = Task $ do
    v <- liftIO $ newEmptyTMVarIO
    children <- forkNode $ do
        endTopic <- runUserNode $ controller go >> subscribed
        _ <- runHandler (\stm -> liftTIO $ atomically $ stm >>= putTMVar v) endTopic
        return ()
    end <- liftIO $ atomically $ takeTMVar v
    liftIO $ killThreadHierarchy children
    return end
  where
    liftTIO :: IO a -> TIO a
    liftTIO = liftIO

forever :: Controller b => b -> Task ()
forever go = Task $ do
    runUserNode $ controller go
    --liftIO $ putStrLn "done"
    --liftIO $ threadDelay $ 2 * 10^6

simulateTask :: Task a -> IO ()
simulateTask = simulateTaskIn world1

simulateTaskIn :: World -> Task a -> IO ()
simulateTaskIn w tsk = simulateNode w (runTask tsk >> return ())


