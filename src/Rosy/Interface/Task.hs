{-# LANGUAGE ScopedTypeVariables, TupleSections, UndecidableInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType, PolyKinds, ConstraintKinds, TypeOperators, TypeFamilies, FlexibleContexts, GADTs #-}

module Rosy.Interface.Task where

import Control.Concurrent.Async
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

import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Set hiding (Proxy(..))
import Type.Compare
import qualified Control.Effect as E

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
    
say :: String -> Task '[] ()
say str = CoreTask $ liftIO $ putStrLn $ str
    
instance Runnable (Task feed a) where
    run t = do
        chan <- liftIO $ newChan
        kill <- liftIO $ newMVar $ return ()
        runNewUserNode $ runTask t (Just . noFeedback) chan kill >> return ()
    
-- | A 'Task', with an initialization step 'init', that runs a controller 'action' until it emits a @Done@ event.
-- Note that the controller may publish more than one @Done@ event. The result type @DoneT action' is 'Either' of the @Done@ events published by the controller.
task :: (SubscribeDones (CtrDones (init,action)),SubscribeFeedbacks (CtrFeedbacks (init,action)),Published init,Controller action) => init -> action -> Task (CtrFeedbacks (init,action)) (DoneT (init,action)) 
task init action = Task init action
    
-- | For a 'Task' with no initialization step.
noInit :: ()
noInit = ()
    
runTask :: Task feedbacks end -> (Eithers feedbacks -> Maybe feed) -> Chan feed -> MVar (IO ()) -> UserNode end
runTask (Task (init::init) (action::action)) feedfun feedchan kill = do
    initialized <- liftIO $ newEmptyMVar
    done <- liftIO $ newEmptyMVar
    children <- forkNewUserNode $ do
        (endTopic,feedTopic) <- do
            published (singleTopic $ return $ Just $ init) >>= lift . runHandler (\stm -> liftTIO $ atomically stm >>= putMVar initialized)
            liftIO $ takeMVar initialized
            controller action
            endTopic <- subscribedDones (Proxy::Proxy (CtrDones (init,action)))
            feedTopic <- subscribedFeedbacks (Proxy::Proxy (CtrFeedbacks (init,action)))
            return (endTopic,feedTopic)
        _ <- lift $ flip runHandler endTopic $ \stm -> liftTIO $ atomically stm >>= putMVar done
        _ <- lift $ flip runHandler feedTopic $ \stm -> liftTIO $ atomically stm >>= \mb -> case (feedfun mb) of
            Nothing -> return ()
            Just v -> writeChan feedchan v
        return ()
    liftIO $ modifyMVar kill $ \m -> return (m >> killThreadHierarchy children,())
    end <- liftIO $ takeMVar done
    liftIO $ killThreadHierarchy children
    return end
runTask (RetTask x) feedfun feedchan kill = return x
runTask (BindTask (m::Task fa a) (f::a->Task fb b)) feedfun feedchan kill = do
        a <- runTask m (feedfun <=< leftUnionEithers (Proxy::Proxy fa) (Proxy::Proxy fb)) feedchan kill
        runTask (f a) (feedfun <=< rightUnionEithers (Proxy::Proxy fa) (Proxy::Proxy fb)) feedchan kill
runTask (SubTask (t1::Task f1 a) (_::Proxy f2)) feedfun feedchan kill = runTask t1 (feedfun <=< subsetEithers (Proxy::Proxy f1) (Proxy::Proxy f2)) feedchan kill
runTask (CoreTask n) feedfun feedchan kill = lift $ n

-- | For calling a 'Task' without the option to cancel it.
noCancel :: () -> Maybe Cancel
noCancel = const Nothing

-- | For calling a 'Task' without listening to its feedback messages.
noFeedback :: a -> ()
noFeedback = const ()

-- | Call a 'Task' from a controller. Receives three additional functions:
--
-- * a function that allows the controller to cancel the 'Task' while in progress;
-- * a function that allows to publish 'Task' progress feedback 'Eithers feed' back to the controller as an event 'see';
-- * a function that publishes the result 'end' of the 'Task' as a controller event 'res'.
call :: (Subscribed when,Published see,Published res) => Task feed end -> (when -> Maybe Cancel) -> (Eithers feed -> see) -> (end -> res) -> Call when see res
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

instance (Typeable when,Typeable see,Typeable res) => Published (Call when see res) where
--    published t = do
--        callv :: TChan Call <- liftIO $ newTChanIO
--        endv :: MVar (Map TypeRep DynChan) <- liftIO $ newMVar Map.empty
--        feedv :: MVar (Map TypeRep DynChan) <- liftIO $ newMVar Map.empty
--        whenv :: MVar (Map TypeRep DynTopicSTM) <- liftIO $ newMVar Map.empty
--        
--        let getRes :: (Published res) => res -> UserNode DynChan
--            getRes (res :: res) = do
--                m <- liftIO $ readMVar endv
--                let ty = typeOf res
--                case Map.lookup ty m of
--                    Just chan -> return chan
--                    Nothing -> do
--                        (chan :: Chan res) <- liftIO $ newChan
--                        liftIO $ modifyMVar endv $ \m -> return (Map.insert ty (DynChan chan) m,())
--                        let endTopic = Topic $ do
--                                x <- lift $ readChan chan
--                                return (return $ Just x,endTopic)
--                        endt <- published endTopic
--                        _ <- lift $ runHandler (lift . atomically) endt
--                        return $ DynChan chan
--        let getSee :: (Published see) => see -> UserNode DynChan
--            getSee (see :: see) = do
--                m <- liftIO $ readMVar feedv
--                let ty = typeOf see
--                case Map.lookup ty m of
--                    Just chan -> return chan
--                    Nothing -> do
--                        (chan :: Chan see) <- liftIO $ newChan
--                        liftIO $ modifyMVar feedv $ \m -> return (Map.insert ty (DynChan chan) m,())
--                        let feedTopic = Topic $ do
--                                x <- lift $ readChan chan
--                                return (return $ Just x,feedTopic)
--                        feedt <- published feedTopic
--                        _ <- lift $ runHandler (lift . atomically) feedt
--                        return $ DynChan chan
--        let getWhen :: (Subscribed when) => when -> UserNode DynTopicSTM
--            getWhen (when :: when) = do
--                m <- liftIO $ readMVar whenv
--                let ty = typeOf when
--                case Map.lookup ty m of
--                    Just chan -> return chan
--                    Nothing -> do
--                        (whenTopic :: Topic TIO (STM when)) <- subscribed
--                        liftIO $ modifyMVar whenv $ \m -> return (Map.insert ty (DynTopicSTM whenTopic) m,())
--                        return $ DynTopicSTM whenTopic
--        let processCall :: Call -> UserNode ()
--            processCall (Call (task :: Task feed end) (whenCancel :: when -> Maybe Cancel) (see:: Eithers feed -> see) (finish :: end -> res)) = do
--                reschan <- liftM (unsafeUnDynChan (Proxy::Proxy res)) $ getRes (error "proxy"::res)
--                seechan <- liftM (unsafeUnDynChan (Proxy::Proxy see)) $ getSee (error "proxy"::see)
--                whent <- liftM (unsafeUnDynTopicSTM (Proxy::Proxy when)) $ getWhen (error "proxy"::when)
--                kill <- liftIO $ newMVar $ return ()
--                end <- do
--                    lift $ flip runHandler_ whent $ \smt -> liftIO $ do
--                        mb <- atomically $ smt
--                        case whenCancel mb of
--                            Nothing -> return ()
--                            Just Cancel -> do
--                                cleanup <- takeMVar kill
--                                cleanup
--                                throw UserInterrupt
--                    runTask task (Just . see) seechan kill
--                liftIO $ writeChan reschan $ finish end
--        
--        _ <- forkUserNodeIO $ do
--            let rec = do
--                    call <- liftIO $ atomically $ readTChan callv
--                    processCall call
--                    rec
--            rec
--        return $ flip fmap t $ \stm -> do
--            mb <- stm
--            case mb of
--                Nothing -> return ()
--                Just call -> writeTChan callv call
                
data PDone a = PDone a

instance Typeable a => Subscribed (PDone a) where
    subscribed = subscribedEvent
instance Typeable a => Published (PDone a) where
    published = publishedEvent

type ParallelK1 f1 f2 = Nub (Sort
                                (((CtrFeedback
                                     (Eithers (Nub (Sort (TMap Feedback f1 :++ TMap Feedback f2))))
                                   :++ '[])
                                  :++ (CtrFeedback
                                         (Eithers
                                            (Nub (Sort (TMap Feedback f1 :++ TMap Feedback f2))))
                                       :++ '[]))
                                 :++ '[])) ~ Nub (Sort (f1 :++ f2))
type ParallelK2 f1 f2 a b = Eithers
                          (Nub
                             (Sort
                                (((CtrDone
                                     (Eithers (Nub (Sort (TMap Feedback f1 :++ TMap Feedback f2))))
                                   :++ '[])
                                  :++ (CtrDone
                                         (Eithers
                                            (Nub (Sort (TMap Feedback f1 :++ TMap Feedback f2))))
                                       :++ '[]))
                                 :++ '[(a, b)]))) ~ (a,b)
type ParallelK3 f1 f2 = (Published (Eithers (Nub (Sort (TMap Feedback f1 :++ TMap Feedback f2)))))
type ParallelK4 f1 f2 a b = (SubscribeDones
                          (Nub
                             (Sort
                                (((CtrDone
                                     (Eithers (Nub (Sort (TMap Feedback f1 :++ TMap Feedback f2))))
                                   :++ '[])
                                  :++ (CtrDone
                                         (Eithers
                                            (Nub (Sort (TMap Feedback f1 :++ TMap Feedback f2))))
                                       :++ '[]))
                                 :++ '[(a, b)]))))
type ParallelK5 f1 f2 = (SubscribeFeedbacks (Nub (Sort (f1 :++ f2))))                            
type ParallelK f1 f2 a b = (UnionEithers (TMap Feedback f1) (TMap Feedback f2),ToFromEithers (Nub (Sort (TMap Feedback f1 :++ TMap Feedback f2))),ToFromEithers (TMap Feedback f1),ToFromEithers (TMap Feedback f2),MapEithers Feedback f1,MapEithers Feedback f2,ParallelK1 f1 f2,ParallelK2 f1 f2 a b,ParallelK3 f1 f2,ParallelK4 f1 f2 a b,ParallelK5 f1 f2,Typeable a,Typeable b)

-- | Run two 'Task's in parallel
parallel :: ParallelK f1 f2 a b => Task f1 a -> Task f2 b -> Task (Union f1 f2) (a,b)
parallel (t1::Task f1 a) (t2::Task f2 b) = task (call t1 Just refeed1 PDone,call t2 Just refeed2 PDone) done
    where
    done :: PDone a -> PDone b -> Done (a,b)
    done (PDone a) (PDone b) = Done (a,b)
    refeed1 :: Eithers f1 -> Maybe (Eithers (Union (TMap Feedback f1) (TMap Feedback f2)))
    refeed1 e = leftUnionEithers (Proxy::Proxy (TMap Feedback f1)) (Proxy::Proxy (TMap Feedback f2)) $ mapEithers Feedback (Proxy::Proxy f1) e
    refeed2 :: Eithers f2 -> Maybe (Eithers (Union (TMap Feedback f1) (TMap Feedback f2)))
    refeed2 = undefined
    --refeed1 = leftUnionEithers (Proxy::Proxy f1) (Proxy::Proxy f2)
    --refeed2 :: Eithers f2 -> Maybe (Eithers (Union f1 f2))
    --refeed2 = rightUnionEithers (Proxy::Proxy f1) (Proxy::Proxy f2)



