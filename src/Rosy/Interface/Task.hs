{-# LANGUAGE ScopedTypeVariables, TupleSections, UndecidableInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType, PolyKinds, ConstraintKinds, TypeOperators, TypeFamilies, FlexibleContexts, GADTs #-}

module Rosy.Interface.Task where

import Control.Concurrent.Async hiding (race,cancel)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.Hierarchy
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (ask)

import Data.Typeable
import Data.Default.Generics
import Data.Map (Map(..))
import qualified Data.Map as Map

--import Data.Type.Bool
--import Data.Type.Equality
--import Data.Type.Set hiding (Proxy(..))
--import Type.Compare
--import qualified Control.Effect as E

import Graphics.Gloss

import Ros.Node
import Rosy.Controller.Core
import Rosy.Interface
import Rosy.Interface.Task.Types

import Ros.Topic.Util as Topic
import Ros.Topic as Topic

import Unsafe.Coerce
import System.IO.Unsafe
import GHC.Conc
import Prelude hiding (init)
    
once :: Published action => action -> Task () ()
once init = task () $ TaskOpts init noCleanup
    
say :: String -> Task () ()
say str = CoreTask $ liftIO $ reportMessage $ str
    
instance (Typeable feed,Typeable a) => Runnable (Task feed a) where
    run t = do
        chan <- liftIO $ newChan
        kill <- liftIO $ newMVar $ return ()
        runNewUserNode $ runTask t (Just . noFeedback) chan kill >> return ()

noInit = ()
noCleanup = ()

-- | Default 'Task' options: no initialization and no cleanup.
taskOpts :: TaskOpts () ()
taskOpts = TaskOpts noInit noCleanup
    
-- | A 'Task', that runs a controller 'action' until it emits a @Done@ event.
task :: (Typeable feed,Typeable end,Published init,Controller action,Published cancel) => action -> TaskOpts init cancel -> Task feed end
task action opts = Task action opts
    
runTask :: Task feed1 end -> (feed1 -> Maybe feed2) -> Chan feed2 -> MVar (IO ()) -> UserNode end
runTask (Task action opts::Task feed1 end) feedfun feedchan kill = do
    initialized <- liftIO $ newEmptyMVar
    done <- liftIO $ newEmptyMVar
    die <- liftIO $ newEmptyMVar
    cancelv <- liftIO $ newEmptyMVar
    children <- forkNewUserNode $ do
        (endTopic,feedTopic) <- do
            published (singleTopic $ return $ Just $ init opts) >>= lift . runHandler (\stm -> liftTIO $ atomically stm >>= putMVar initialized)
            liftIO $ takeMVar initialized
            controller action
            endTopic <- subscribedProxy (Proxy::Proxy (Done end))
            feedTopic <- subscribedProxy (Proxy::Proxy (Feedback feed1))
            return (endTopic,feedTopic)
        let cancelTopic = Topic $ do
                Cancel <- lift $ takeMVar cancelv
                return (return $ Just $ cleanup opts,haltTopic)
        published cancelTopic >>= lift . runHandler (\stm -> liftTIO $ atomically stm >>= putMVar die)
        _ <- lift $ flip runHandler endTopic $ \stm -> liftTIO $ atomically stm >>= putMVar done
        _ <- lift $ flip runHandler feedTopic $ \stm -> liftTIO $ atomically stm >>= \mb -> case (feedfun $ unFeedback mb) of
            Nothing -> return ()
            Just v -> writeChan feedchan v
        return ()
    liftIO $ modifyMVar kill $ \m -> return (putMVar cancelv Cancel >> m >> takeMVar die >> killThreadHierarchy children,())
    Done end <- liftIO $ takeMVar done
    liftIO $ killThreadHierarchy children
    return end
runTask (RetTask x) feedfun feedchan kill = return x
runTask (BindTask (m::Task fa a) (f::a->Task fa b)) feedfun feedchan kill = do
        a <- runTask m (feedfun ) feedchan kill
        runTask (f a) (feedfun ) feedchan kill
runTask (SubTask upcast (t1::Task f1 a)) feedfun feedchan kill = runTask t1 (feedfun <=< upcast) feedchan kill
runTask (CoreTask n) feedfun feedchan kill = lift $ n

noCancel () = Nothing
noFeedback _ = ()
noResponse _ = ()

-- | Default 'call' options: no cancellation, no feedback and no response.
callOpts :: CallOpts () feed () end ()
callOpts = CallOpts noCancel noFeedback noResponse

-- | Call a 'Task' from a controller. 
call :: (Subscribed when,Published see,Published res) => Task feed end -> CallOpts when feed see end res -> Call 
call = Call

data DynTopicSTM where
  DynTopicSTM :: Typeable a => Topic TIO (STM a) -> DynTopicSTM

data DynChan where
  DynChan :: Typeable a => Chan a -> DynChan
  
data DynTChan where
  DynTChan :: Typeable a => TChan a -> DynTChan
  
unsafeUnDynChan :: Proxy a -> DynChan -> Chan a
unsafeUnDynChan p (DynChan c) = unsafeCoerce c

unsafeUnDynTChan :: Proxy a -> DynTChan -> TChan a
unsafeUnDynTChan p (DynTChan c) = unsafeCoerce c

unsafeUnDynTopicSTM :: Proxy a -> DynTopicSTM -> Topic TIO (STM a)
unsafeUnDynTopicSTM p (DynTopicSTM c) = unsafeCoerce c

instance Published Call where
    published t = do
        callv :: TChan Call <- liftIO $ newTChanIO
        threadv :: MVar ThreadId <- liftIO $ newEmptyMVar
        endv :: MVar (Map TypeRep DynChan) <- liftIO $ newMVar Map.empty
        feedv :: MVar (Map TypeRep DynChan) <- liftIO $ newMVar Map.empty
        whenv :: MVar (Map TypeRep DynTopicSTM) <- liftIO $ newMVar Map.empty
        
        let getRes :: (Published res) => res -> UserNode DynChan
            getRes (res :: res) = do
                m <- liftIO $ readMVar endv
                let ty = typeOf res
                case Map.lookup ty m of
                    Just chan -> return chan
                    Nothing -> do
                        (chan :: Chan res) <- liftIO $ newChan
                        liftIO $ modifyMVar endv $ \m -> return (Map.insert ty (DynChan chan) m,())
                        let endTopic = Topic $ do
                                x <- lift $ readChan chan
                                return (return $ Just x,endTopic)
                        endt <- published endTopic
                        _ <- lift $ runHandler (lift . atomically) endt
                        return $ DynChan chan
        let getSee :: (Published see) => see -> UserNode DynChan
            getSee (see :: see) = do
                m <- liftIO $ readMVar feedv
                let ty = typeOf see
                case Map.lookup ty m of
                    Just chan -> return chan
                    Nothing -> do
                        (chan :: Chan see) <- liftIO $ newChan
                        liftIO $ modifyMVar feedv $ \m -> return (Map.insert ty (DynChan chan) m,())
                        let feedTopic = Topic $ do
                                x <- lift $ readChan chan
                                return (return $ Just x,feedTopic)
                        feedt <- published feedTopic
                        _ <- lift $ runHandler (lift . atomically) feedt
                        return $ DynChan chan
        let getWhen :: (Subscribed when) => when -> UserNode DynTopicSTM
            getWhen (when :: when) = do
                m <- liftIO $ readMVar whenv
                let ty = typeOf when
                case Map.lookup ty m of
                    Just chan -> return chan
                    Nothing -> do
                        (whenTopic :: Topic TIO (STM when)) <- subscribed
                        liftIO $ modifyMVar whenv $ \m -> return (Map.insert ty (DynTopicSTM whenTopic) m,())
                        return $ DynTopicSTM whenTopic
        let processCall :: Call -> UserNode ()
            processCall (Call (task :: Task feed end) (CallOpts (whenCancel :: when -> Maybe Cancel) (see::feed -> see) (finish :: end -> res))) = do
                reschan <- liftM (unsafeUnDynChan (Proxy::Proxy res)) $ getRes (error "proxy"::res)
                seechan <- liftM (unsafeUnDynChan (Proxy::Proxy see)) $ getSee (error "proxy"::see)
                whent <- liftM (unsafeUnDynTopicSTM (Proxy::Proxy when)) $ getWhen (error "proxy"::when)
                kill <- liftIO $ newMVar $ return ()
                end <- do
                    lift $ flip runHandler_ whent $ \smt -> liftIO $ do
                        mb <- atomically $ smt
                        case whenCancel mb of
                            Nothing -> return ()
                            Just Cancel -> do
                                cleanup <- takeMVar kill
                                tid <- takeMVar threadv
                                --putStrLn $ "kill yourself " ++ show tid
                                cleanup
                                killThread tid
                    runTask task (Just . see) seechan kill
                liftIO $ writeChan reschan $ finish end
                tid <- liftIO $ takeMVar threadv
                liftIO $ killThread tid
        
        _ <- forkUserNodeIO $ do
            let rec = do
                    call <- liftIO $ atomically $ readTChan callv
                    tid <- forkUserNodeIO $ processCall call
                    --liftIO $ putStrLn $ "call " ++ show tid
                    liftIO $ putMVar threadv tid
                    rec
            rec
        return $ flip fmap t $ \stm -> do
            mb <- stm
            case mb of
                Nothing -> return ()
                Just call -> writeTChan callv call
                
data PDone a = PDone a

instance Typeable a => Subscribed (PDone a) where
    subscribed = subscribedEvent
instance Typeable a => Published (PDone a) where
    published = publishedEvent

-- | Run two 'Task's in parallel
parallel :: (Typeable f1,Typeable f2,Typeable a,Typeable b) => Task f1 a -> Task f2 b -> Task (Either f1 f2) (a,b)
parallel (t1::Task f1 a) (t2::Task f2 b) = task done $ TaskOpts (call t1 (CallOpts Just refeed1 PDone),call t2 (CallOpts Just refeed2 PDone)) ()
    where
    done :: PDone a -> PDone b -> Done (a,b)
    done (PDone a) (PDone b) = Done (a,b)
    refeed1 :: f1 -> Feedback (Either f1 f2)
    refeed1 = Feedback . Left
    refeed2 :: f2 -> Feedback (Either f1 f2)
    refeed2 = Feedback . Right

parallel_ :: (Typeable a,Typeable b) => Task () a -> Task () b -> Task () (a,b)
parallel_ t1 t2 = subTask (const Nothing) (parallel t1 t2)

-- | Run two 'Task's in parallel and stop when first finishes
race :: (Typeable f1,Typeable f2,Typeable a,Typeable b) => Task f1 a -> Task f2 b -> Task (Either f1 f2) (Either a b)
race (t1::Task f1 a) (t2::Task f2 b) = task (done1,done2) $ TaskOpts (call t1 (CallOpts Just refeed1 PDone),call t2 (CallOpts Just refeed2 PDone)) ()
    where
    done1 :: PDone a -> Done (Either a b)
    done1 (PDone a) = Done $ Left a
    done2 :: PDone b -> Done (Either a b)
    done2 (PDone b) = Done $ Right b
    refeed1 :: f1 -> Feedback (Either f1 f2)
    refeed1 = Feedback . Left
    refeed2 :: f2 -> Feedback (Either f1 f2)
    refeed2 = Feedback . Right

race_ :: (Typeable a,Typeable b) => Task () a -> Task () b -> Task () (Either a b)
race_ t1 t2 = subTask (const Nothing) (race t1 t2)




