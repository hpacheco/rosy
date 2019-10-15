{-# LANGUAGE MultiParamTypeClasses, CPP, DeriveGeneric, GeneralizedNewtypeDeriving, UndecidableInstances, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

module Rosy.Controller.Core where

import Data.UUID.V4 (nextRandom)
import Data.Maybe

import qualified Data.Default.Generics as D
import Data.Typeable
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Lens.Family.TH (makeLensesBy)
import Lens.Family (view, set)

import Ros.Node
import Ros.Topic.Util (TIO,(<+>))
import qualified Ros.Topic as Topic
import qualified Ros.Topic.Util as Topic
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Ros.Rate

import Rosy.Util

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State (StateT(..),gets)
import qualified Control.Monad.State as State
import Control.Monad.Reader (ReaderT(..),asks)
import qualified Control.Monad.Reader as Reader
import GHC.Conc.Sync hiding (modifyMVar_)
import Control.Concurrent.STM
import Control.Concurrent

import Unsafe.Coerce
import System.IO.Unsafe
import System.Random

liftTIO :: IO a -> TIO a
liftTIO = liftIO

#if defined(ghcjs_HOST_OS)
import Graphics.Gloss.Interface.Environment
#else
#endif

#if defined(ghcjs_HOST_OS)
reportMessage :: String -> IO ()
reportMessage msg = do
    reportRuntimeMessage (msg++"\n")
#else
reportMessage :: String -> IO ()
reportMessage msg = putStrLn msg
#endif

halt :: IO a
halt = do
    forever (threadDelay maxBound)
    return (error "halt")

haltTopic :: Topic TIO a
haltTopic = Topic $ do
    a <- liftIO $ halt
    return (a,haltTopic)
    
singleTopic :: a -> Topic TIO a
singleTopic x = Topic.cons x haltTopic

accelerateTopic :: Double -> Topic TIO a -> Node (Topic TIO a)
accelerateTopic hz t = do
    mvar <- liftIO $ newEmptyMVar 
    _ <- flip runHandler t $ \a -> liftIO $ tryTakeMVar mvar >> putMVar mvar a
    return $ Topic.topicRate hz $ Topic.repeatM $ liftIO $ readMVar mvar

defaultRate :: Double
defaultRate = 10

fmap2 :: (Functor f,Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f = fmap (fmap f)

fmap3 :: (Functor f,Functor g,Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 f = fmap (fmap (fmap f))

-- * User Events

data UserEvent = UserEvent
    { userEventChan :: TChan ()
    , userEventTopic :: Topic TIO ()
    }

newUserEvents :: IO (MVar (Map TypeRep UserEvent))
newUserEvents = newMVar Map.empty

getUserEvent :: TypeRep -> UserNode UserEvent
getUserEvent ty = do
    (e,tid) <- getUserEvent' ty
    lift $ mapM_ (addCleanup . killThread) tid
    return e

getUserEvent' :: TypeRep -> UserNode (UserEvent,Maybe ThreadId)
getUserEvent' ty = do
    v <- asks userEvents
    ts <- lift $ getThreads
    liftIO $ modifyMVar v $ \events -> do
        case Map.lookup ty events of
            Just e -> return (events,(e,Nothing))
            Nothing -> do
                c <- newTChanIO
                let stream = Topic $ do { x <- liftIO (atomically (readTChan c)); return (x, stream) }
                (stream',tid) <- runReaderT (Topic.shareUnsafe stream) ts
                let e = UserEvent c stream'
                return (Map.insert ty e events,(e,Just tid))

publishedEvent :: Typeable a => Topic TIO (STM (Maybe a)) -> UserNode (Topic TIO (STM ()))
publishedEvent = publishedEventType undefined
    
publishedEventType :: Typeable a => a -> Topic TIO (STM (Maybe a)) -> UserNode (Topic TIO (STM ()))
publishedEventType ty topic = do
    e <- getUserEvent (typeOf ty)
    let write m = do
            mb <- m
            case mb of
                Nothing -> return ()
                Just a -> writeTChan (userEventChan e) (unsafeCoerce a)
    return $ fmap write topic

subscribedEvent :: Typeable a => UserNode (Topic TIO (STM a))
subscribedEvent = subscribedSTM $ subscribedEventType undefined

subscribedEventType :: Typeable a => a -> UserNode (Topic TIO a)
subscribedEventType ty = do
    e <- getUserEvent (typeOf ty)
    return $ fmap unsafeCoerce (userEventTopic e)

-- * User Memory

type UserMemory = TVar ()

newUserMemories :: IO (MVar (Map TypeRep UserMemory))
newUserMemories = newMVar Map.empty

getUserMemory :: Typeable a => a -> UserNode (TVar a)
getUserMemory a = asks userMemories >>= \v -> liftIO $ modifyMVar v $ \memories -> do
    let ty = typeOf a
    case Map.lookup ty memories of
        Just v -> return (memories,unsafeCoerce v)
        Nothing -> do
            v <- newTVarIO a
            return (Map.insert ty (unsafeCoerce v) memories,v)

publishedMemory :: (D.Default a,Typeable a) => Topic TIO (STM (Maybe a)) -> UserNode (Topic TIO (STM ()))
publishedMemory t = do
    tv <- getUserMemory D.def
    let write m = do
            mb <- m
            case mb of
                Nothing -> return ()
                Just a -> writeTVar tv a
    return $ fmap write t

subscribedMemory :: (D.Default a,Typeable a) => UserNode (Topic TIO (STM a))
subscribedMemory = do
    tv <- getUserMemory D.def
    return $ Topic.topicRate defaultRate $ Topic.repeat $ readTVar tv

data Memory a = Memory a
  deriving (Show, Eq, Ord, Typeable, G.Generic)

instance D.Default a => D.Default (Memory a) where
    def = Memory D.def

-- * Controllers

data UserState = UserState
    { userEvents   :: MVar (Map TypeRep UserEvent)
    , userMemories :: MVar (Map TypeRep UserMemory)
    } deriving (Typeable, G.Generic)
type UserNode = ReaderT UserState Node 

newUserState :: IO UserState
newUserState = do
    es <- newUserEvents
    ms <- newUserMemories
    return $ UserState es ms

runUserNode :: UserNode a -> Node a
runUserNode n = do
    st <- liftIO newUserState
    a <- runReaderT n st
    return a

-- | Command the robot to speak some sentence.
data Say = Say String
  deriving (Show, Eq, Ord, Typeable, G.Generic)

class Subscribed a where
    subscribed :: UserNode (Topic TIO (STM a))

instance (D.Default a,Typeable a) => Subscribed (Memory a) where
    subscribed = subscribedMemory

subscribedROS :: Node (Topic TIO a) -> UserNode (Topic TIO (STM a))
subscribedROS n = lift $ subscribedSTM n

subscribedSTM :: Monad n => n (Topic TIO a) -> n (Topic TIO (STM a))
subscribedSTM n = do
    t <- n
    return $ fmap return t

instance (Subscribed a,Subscribed b) => Subscribed (a,b) where
    subscribed = do
        a <- subscribed
        b <- subscribed
        let merge (ma,mb) = do { a <- ma; b <- mb; return (a,b) }
        return $ fmap merge $ a `Topic.bothNew` b

instance (Subscribed a,Subscribed b,Subscribed c) => Subscribed (a,b,c) where
    subscribed = fmap3 (\(a,(b,c)) -> (a,b,c)) subscribed

instance (Subscribed a,Subscribed b,Subscribed c,Subscribed d) => Subscribed (a,b,c,d) where
    subscribed = fmap3 (\(a,(b,(c,d))) -> (a,b,c,d)) subscribed

instance (Subscribed a,Subscribed b,Subscribed c,Subscribed d,Subscribed e) => Subscribed (a,b,c,d,e) where
    subscribed = fmap3 (\(a,(b,(c,(d,e)))) -> (a,b,c,d,e)) subscribed

instance (Subscribed a,Subscribed b,Subscribed c,Subscribed d,Subscribed e,Subscribed f) => Subscribed (a,b,c,d,e,f) where
    subscribed = fmap3 (\(a,(b,(c,(d,(e,f))))) -> (a,b,c,d,e,f)) subscribed

instance (Subscribed a,Subscribed b,Subscribed c,Subscribed d,Subscribed e,Subscribed f,Subscribed g) => Subscribed (a,b,c,d,e,f,g) where
    subscribed = fmap3 (\(a,(b,(c,(d,(e,(f,g)))))) -> (a,b,c,d,e,f,g)) subscribed

instance (Subscribed a,Subscribed b,Subscribed c,Subscribed d,Subscribed e,Subscribed f,Subscribed g,Subscribed h) => Subscribed (a,b,c,d,e,f,g,h) where
    subscribed = fmap3 (\(a,(b,(c,(d,(e,(f,(g,h))))))) -> (a,b,c,d,e,f,g,h)) subscribed

instance (Subscribed a,Subscribed b,Subscribed c,Subscribed d,Subscribed e,Subscribed f,Subscribed g,Subscribed h,Subscribed i) => Subscribed (a,b,c,d,e,f,g,h,i) where
    subscribed = fmap3 (\(a,(b,(c,(d,(e,(f,(g,(h,i)))))))) -> (a,b,c,d,e,f,g,h,i)) subscribed

instance (Subscribed a,Subscribed b) => Subscribed (Either a b) where
    subscribed = do
        a <- subscribed
        b <- subscribed
        let interleave = either (fmap Left) (fmap Right)
        return $ fmap interleave $ a <+> b

instance (Subscribed a) => Subscribed (Maybe a) where
    subscribed = fmap3 (either Just (\() -> Nothing)) subscribed
        
instance Subscribed () where
    subscribed = return $ Topic.topicRate defaultRate $ Topic.repeat (return ())

instance Subscribed StdGen where
    subscribed = return $ Topic.topicRate defaultRate $ Topic.repeatM $ liftM return $ lift newStdGen
    
class Published a where
    -- the output topic preserves the periodicity of the input topic
    -- the output is a topic of transactions that publish each value
    published :: Topic TIO (STM (Maybe a)) -> UserNode (Topic TIO (STM ()))
    
instance (D.Default a,Typeable a) => Published (Memory a) where
    published = publishedMemory
    
-- writes to a transactional buffer, and buffer gets advertised to ROS
publishedROS :: (Topic TIO a -> Node ()) -> Topic TIO (STM (Maybe a)) -> UserNode (Topic TIO (STM ()))
publishedROS adv t = lift $ do
    chan <- liftIO $ newTChanIO
    let chanTopic = Topic $ do
            x <- lift $ atomically $ readTChan chan
            return (x,chanTopic)
    adv chanTopic
    let put m = do
            mb <- m
            case mb of
                Nothing -> return ()
                Just a -> writeTChan chan a
    return $ fmap put t
    
mergeT :: Monad m => Topic TIO (m ()) -> Topic TIO (m ()) -> (Topic TIO (m ()))
mergeT t1 t2 = fmap (uncurry (>>)) $ Topic.bothNew t1 t2
    
instance Published Say where
    published = publishedROS $ \t -> runHandler (\(Say str) -> liftIO $ reportMessage str) t >> return ()

instance (Published a,Published b) => Published (a,b) where
    published tab = do
        pair <- liftIO $ newTVarIO (error "no pair value")
        (tab1,tab23) <- lift $ nodeTIO $ Topic.tee tab
        (tab2,tab3) <- lift $ nodeTIO $ Topic.tee tab23
        let t' = flip fmap tab1 $ \mx -> mx >>= writeTVar pair
        let ta = flip fmap tab2 $ const $ liftM (fmap fst) $ readTVar pair
        let tb = flip fmap tab3 $ const $ liftM (fmap snd) $ readTVar pair
        ta' <- published ta
        tb' <- published tb
        return $ mergeT t' (mergeT ta' tb')

instance (Published a,Published b,Published c) => Published (a,b,c) where
    published t = published $ fmap3 (\(a,b,c) -> (a,(b,c))) t
    
instance (Published a,Published b,Published c,Published d) => Published (a,b,c,d) where
    published t = published $ fmap3 (\(a,b,c,d) -> (a,(b,(c,d)))) t
    
instance (Published a,Published b,Published c,Published d,Published e) => Published (a,b,c,d,e) where
    published t = published $ fmap3 (\(a,b,c,d,e) -> (a,(b,(c,(d,e))))) t

instance (Published a,Published b,Published c,Published d,Published e,Published f) => Published (a,b,c,d,e,f) where
    published t = published $ fmap3 (\(a,b,c,d,e,f) -> (a,(b,(c,(d,(e,f)))))) t

instance (Published a,Published b,Published c,Published d,Published e,Published f,Published g) => Published (a,b,c,d,e,f,g) where
    published t = published $ fmap3 (\(a,b,c,d,e,f,g) -> (a,(b,(c,(d,(e,(f,g))))))) t

instance (Published a,Published b,Published c,Published d,Published e,Published f,Published g,Published h) => Published (a,b,c,d,e,f,g,h) where
    published t = published $ fmap3 (\(a,b,c,d,e,f,g,h) -> (a,(b,(c,(d,(e,(f,(g,h)))))))) t

instance (Published a,Published b,Published c,Published d,Published e,Published f,Published g,Published h,Published i) => Published (a,b,c,d,e,f,g,h,i) where
    published t = published $ fmap3 (\(a,b,c,d,e,f,g,h,i) -> (a,(b,(c,(d,(e,(f,(g,(h,i))))))))) t

instance (Published a,Published b) => Published (Either a b) where
    published tab = do
        pair <- liftIO $ newTVarIO (error "no sum value")
        (tab1,tab23) <- lift $ nodeTIO $ Topic.tee tab
        (tab2,tab3) <- lift $ nodeTIO $ Topic.tee tab23
        let t' = flip fmap tab1 (\mx -> mx >>= writeTVar pair)
        let ta = flip fmap tab2 $ const $ do
                mbe <- readTVar pair
                case mbe of
                    Nothing -> return Nothing
                    Just (Left a) -> return $ Just a
                    Just (Right b) -> return Nothing
        let tb = flip fmap tab3 $ const $ do
                mbe <- readTVar pair
                case mbe of
                    Nothing -> return Nothing
                    Just (Left a) -> return Nothing
                    Just (Right b) -> return $ Just b
        ta' <- published ta
        tb' <- published tb
        return $ mergeT t' (mergeT ta' tb')

instance (Published a) => Published (Maybe a) where
    published t = do
        published $ fmap2 join t

instance Published () where
    published t = return $ fmap2 (const ()) t
    
instance (Subscribed a,Published b) => Published (a -> b) where
    published tf = do
        ta <- subscribed
        let apply mf ma = do
                mbf <- mf
                case mbf of
                    Nothing -> return Nothing
                    Just f -> liftM (Just . f) ma
        published $ fmap (uncurry apply) $ Topic.bothNew tf ta

-- * Controllers

class Controller a where
    controller :: a -> UserNode ()
    
instance {-# OVERLAPPABLE #-} Published b => Controller b where
    controller b = do
        go <- published $ Topic.topicRate defaultRate $ Topic.repeat (return $ Just b)
        _ <- lift $ runHandler (lift . atomically) go
        return ()

instance Controller a => Controller [a] where
    controller = mapM_ controller

instance (Controller a,Controller b) => Controller (a,b) where
    controller (a,b) = controller a >> controller b

instance (Controller a,Controller b,Controller c) => Controller (a,b,c) where
    controller (a,b,c) = controller a >> controller b >> controller c

instance (Controller a,Controller b,Controller c,Controller d) => Controller (a,b,c,d) where
    controller (a,b,c,d) = controller a >> controller b >> controller c >> controller d

instance (Controller a,Controller b,Controller c,Controller d,Controller e) => Controller (a,b,c,d,e) where
    controller (a,b,c,d,e) = controller a >> controller b >> controller c >> controller d >> controller e

instance (Controller a,Controller b,Controller c,Controller d,Controller e,Controller f) => Controller (a,b,c,d,e,f) where
    controller (a,b,c,d,e,f) = controller a >> controller b >> controller c >> controller d >> controller e >> controller f

instance (Controller a,Controller b,Controller c,Controller d,Controller e,Controller f,Controller g) => Controller (a,b,c,d,e,f,g) where
    controller (a,b,c,d,e,f,g) = controller a >> controller b >> controller c >> controller d >> controller e >> controller f >> controller g

instance (Controller a,Controller b,Controller c,Controller d,Controller e,Controller f,Controller g,Controller h) => Controller (a,b,c,d,e,f,g,h) where
    controller (a,b,c,d,e,f,g,h) = controller a >> controller b >> controller c >> controller d >> controller e >> controller f >> controller g >> controller h

instance (Controller a,Controller b,Controller c,Controller d,Controller e,Controller f,Controller g,Controller h,Controller i) => Controller (a,b,c,d,e,f,g,h,i) where
    controller (a,b,c,d,e,f,g,h,i) = controller a >> controller b >> controller c >> controller d >> controller e >> controller f >> controller g >> controller h >> controller i

roshome = "/mobile_base"


