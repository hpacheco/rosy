{-# LANGUAGE ScopedTypeVariables, TupleSections, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Rosy.Interface where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent
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
import Rosy.Robot.State
import Rosy.Viewer.State
import Rosy.Controller.Core
import Rosy.Robot.Kobuki
import Rosy.Viewer.Core

import Ros.Topic.Util as Topic
import Ros.Topic as Topic

import Unsafe.Coerce
import System.IO.Unsafe

control :: Controller a => a -> IO ()
control n = runNode "rosy-simulator" (runUserNode $ controller n)

controlTask :: Task end -> IO ()
controlTask t = runNode "rosy-simulator" (runTask t)

startNode :: Node () -> WorldState -> IO ()
startNode n w = runNode "rosy-simulator" $ do
    runViewerNodes w
    runRobotNodes w
    n

-- | The main function that produces a Rosy program.
-- It receives a robot 'Controller' that does the actual job of interacting with your robot.
simulate :: Controller a => a -> IO ()
simulate = simulateIn world1

simulateTask :: Task end -> IO ()
simulateTask = simulateTaskIn world1

-- | The main function that produces a Rosy program.
-- It receives a robot 'Controller' that does the actual job of interacting with your robot.
-- It also receives a world in which the robot navigates.
simulateIn :: Controller a => World -> a -> IO ()
simulateIn w n = simulateNode w $ runUserNode $ controller n

simulateTaskIn :: World -> Task end -> IO ()
simulateTaskIn w n = simulateNode w $ runTask n >> return ()

simulateNode :: World -> Node () -> IO ()
simulateNode world n = do
    w <- newWorldState world
    concurrently_ (startNode n w) (runViewer w)

-- | A monad for performing multiple robot tasks, returning a final event
newtype Task end = Task { runTask :: Node end }
    deriving (Applicative,Functor,Monad)

--data TaskState = TaskState
--    { taskSubscriptions :: MVar (Map TypeRep TaskSubscription)
--    , taskCleanup :: MVar (IO ())
--    } deriving (Typeable, G.Generic)
--
--data TaskSubscription = TaskSubscription
--    { taskSubscriptionChan :: TChan () -- channel that buffers received messages
--    , taskSubscriptionTopic :: Topic TIO () -- shared topic that reads from channel since start of subscription
--    }

--getTaskSubscription :: TypeRep -> Task TaskSubscription
--getTaskSubscription ty = do
--    (e,tid) <- getTaskSubscription' ty
--    lift $ mapM_ (addTaskCleanup . killThread) tid
--    return e
--
--getTaskSubscription' :: TypeRep -> Task (TaskSubscription,Maybe ThreadId)
--getTaskSubscription' ty = do
--    v <- asks taskSubscriptions
--    ts <- lift2 $ getThreads
--    liftIO $ modifyMVar v $ \events -> do
--        case Map.lookup ty events of
--            Just e -> return (events,(e,Nothing))
--            Nothing -> do
--                c <- newTChanIO
--                let stream = Topic $ do { x <- liftIO (atomically (readTChan c)); return (x, stream) }
--                (stream',tid) <- runReaderT (Topic.shareUnsafe stream) ts
--                let e = TaskSubscription c stream'
--                return (Map.insert ty e events,(e,Just tid))
--
--instance Subscribed UserNode a => Subscribed Task a where
--    subscribed = subscribedTaskType undefined

--subscribedTaskType :: Subscribed UserNode a => a -> Task (Topic TIO (STM a))
--subscribedTaskType ty = Task $ do
--    sub <- getTaskSubscription (typeOf ty)
--    topic <- lift $ subscribed
--    let feed t = do
--        (a,t') <- runTopic t
--        liftIO $ atomically $ writeTChan (taskSubscriptionChan sub) a
--        feed t'
--    nodeTIO $ forkTIO $ feed topic
--    return $ taskSubscriptionTopic sub
--        
--instance Published UserNode a => Published Task a where
--    published t = lift $ published t    

--runTask :: Task end -> Node end
--runTask (Task m) st = do
--    subs <- liftIO $ newMVar Map.empty
--    cleanup <- liftIO $ newMVar (return ())
--    end <- runReaderT m subs
--    liftIO $ readMVar cleanup >>= id
--    return end
    
--both :: Task a -> Task b -> Task (a,b)
--both ta tb = Task $ do
--    s <- ask >>= readMVar
--    (sa,sb) <- liftIO $ forkTaskSubscriptions s
--    a <- runTaskWith ta sa
--    b <- runTaskWith tb sb
--    return (a,b)

data Done end = Done end
  deriving (Show,Eq,Ord,Typeable)
instance Typeable end => Subscribed (Done end) where
    subscribed = subscribedEvent
instance Typeable end => Published (Done end) where
    published = publishedEvent

-- | A task that runs a controller until it emits a @Done@ event, with a return value of type @end@.
task' :: (Typeable end,Controller action) => action -> Task end
task' action = Task $ do
    v <- liftIO $ newEmptyTMVarIO
    children <- forkNode $ do
        endTopic <- runUserNode $ do
            controller action
            subscribed
        _ <- runHandler (\stm -> liftTIO $ atomically $ stm >>= putTMVar v) endTopic
        return ()
    Done end <- liftIO $ atomically $ takeTMVar v
    liftIO $ killThreadHierarchy children
    return end
    
-- | A 'task', with an initialization step.
task :: (Typeable end,Published init,Controller action) => init -> action -> Task end 
task init action = Task $ do
    initialized <- liftIO $ newEmptyTMVarIO
    done <- liftIO $ newEmptyTMVarIO
    children <- forkNode $ do
        endTopic <- runUserNode $ do
            published (singleTopic $ return $ Just $ init) >>= lift . runHandler (\stm -> liftTIO $ atomically $ stm >>= putTMVar initialized)
            liftIO $ atomically $ takeTMVar initialized
            controller action
            subscribed
        _ <- runHandler (\stm -> liftTIO $ atomically $ stm >>= putTMVar done) endTopic
        return ()
    Done end <- liftIO $ atomically $ takeTMVar done
    liftIO $ killThreadHierarchy children
    return end



