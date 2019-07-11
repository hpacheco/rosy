{-# LANGUAGE CPP, DeriveGeneric, GeneralizedNewtypeDeriving, UndecidableInstances, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

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
import Ros.Topic.Util ((<+>))
import qualified Ros.Topic as Topic
import qualified Ros.Topic.Util as Topic
import Data.Time.Clock
import Ros.Rate

import Rosy.Util

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State as State
import GHC.Conc.Sync
import Control.Concurrent.STM
import Control.Concurrent

import Unsafe.Coerce
import System.IO.Unsafe

#if defined(ghcjs_HOST_OS)
import CodeWorld
#else
#endif

#if defined(ghcjs_HOST_OS)
reportMessage :: String -> IO ()
reportMessage msg = do
    reportRuntimeMessage msg
#else
reportMessage :: String -> IO ()
reportMessage msg = putStrLn msg
#endif


accelerate :: Double -> Topic IO a -> Node (Topic IO a)
accelerate hz t = do
    mvar <- liftIO $ newEmptyMVar 
    _ <- flip runHandler t $ \a -> tryTakeMVar mvar >> putMVar mvar a
    return $ Topic.topicRate hz $ Topic.repeatM $ readMVar mvar

defaultRate :: Double
defaultRate = 10

fmap2 :: (Functor f,Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f = fmap (fmap f)

fmap3 :: (Functor f,Functor g,Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 f = fmap (fmap (fmap f))

-- * User Events

data UserEvent = UserEvent
    { userEventChan :: TChan ()
    , userEventTopic :: Topic IO ()
    }

userEvents :: MVar (Map TypeRep UserEvent)
{-# NOINLINE userEvents #-}
userEvents = unsafePerformIO (newMVar Map.empty)

getUserEvent :: TypeRep -> IO UserEvent
getUserEvent ty = modifyMVar userEvents $ \events -> do
    case Map.lookup ty events of
        Just e -> return (events,e)
        Nothing -> do
            c <- newTChanIO
            let stream = Topic $ do { x <- atomically (readTChan c); return (x, stream) }
            stream' <- liftIO $ Topic.share stream
            let e = UserEvent c stream'
            return (Map.insert ty e events,e)

publishedEvent :: Typeable a => Topic IO (STM (Maybe a)) -> Node (Topic IO (STM ()))
publishedEvent = publishedEventType undefined
    
publishedEventType :: Typeable a => a -> Topic IO (STM (Maybe a)) -> Node (Topic IO (STM ()))
publishedEventType ty topic = do
    e <- liftIO $ getUserEvent (typeOf ty)
    let write m = do
            mb <- m
            case mb of
                Nothing -> return ()
                Just a -> writeTChan (userEventChan e) (unsafeCoerce a)
    return $ fmap write topic

subscribedEvent :: Typeable a => Node (Topic IO (STM a))
subscribedEvent = subscribedROS $ subscribedEventType undefined

subscribedEventType :: Typeable a => a -> Node (Topic IO a)
subscribedEventType ty = do
        e <- liftIO $ getUserEvent (typeOf ty)
        return $ fmap unsafeCoerce (userEventTopic e)

-- * User Memory

type UserMemory = TVar ()

userMemories :: MVar (Map TypeRep UserMemory)
{-# NOINLINE userMemories #-}
userMemories = unsafePerformIO (newMVar Map.empty)

getUserMemory :: Typeable a => a -> IO (TVar a)
getUserMemory a = modifyMVar userMemories $ \memories -> do
    let ty = typeOf a
    case Map.lookup ty memories of
        Just v -> return (memories,unsafeCoerce v)
        Nothing -> do
            v <- newTVarIO a
            return (Map.insert ty (unsafeCoerce v) memories,v)

publishedMemory :: (D.Default a,Typeable a) => Topic IO (STM (Maybe a)) -> Node (Topic IO (STM ()))
publishedMemory t = do
    tv <- liftIO $ getUserMemory D.def
    let write m = do
            mb <- m
            case mb of
                Nothing -> return ()
                Just a -> writeTVar tv a
    return $ fmap write t

subscribedMemory :: (D.Default a,Typeable a) => Node (Topic IO (STM a))
subscribedMemory = do
    tv <- liftIO $ getUserMemory D.def
    return $ Topic.topicRate defaultRate $ Topic.repeat (readTVar tv)

-- * Controllers

-- | Command the robot to speak some sentence.
data Say = Say String
  deriving (Show, Eq, Ord, Typeable, G.Generic)

class Subscribed a where
    subscribed :: Node (Topic IO (STM a))

subscribedROS :: Node (Topic IO a) -> Node (Topic IO (STM a))
subscribedROS n = do
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
    
-- | The current time in hours, minutes and seconds.
data Clock = Clock
    { hours   :: Int
    , minutes :: Int
    , seconds :: Int
    } deriving (Show, Eq, Ord, Typeable, G.Generic)
    
$(makeLensesBy (Just . (++"Lens")) ''Clock)

clockFromUTCTime :: UTCTime -> Clock
clockFromUTCTime utc = Clock h m s
    where
    diff = utctDayTime utc
    h = remBy 24 $ quotBy 3600 diff
    m = remBy 60 $ quotBy 60 diff
    s = remBy 60 $ quotBy 1 diff

instance D.Default Clock

instance Subscribed Clock where
    subscribed = return $ Topic.topicRate defaultRate $ Topic.repeatM $ liftM (return . clockFromUTCTime) getCurrentTime
    
class Published a where
    published :: Topic IO (STM (Maybe a)) -> Node (Topic IO (STM ()))
    
-- writes to a transactional buffer, and buffer gets advertised to ROS
publishedROS :: (Topic IO a -> Node ()) -> Topic IO (STM (Maybe a)) -> Node (Topic IO (STM ()))
publishedROS adv t = do
    chan <- liftIO $ newTChanIO
    let chanTopic = Topic $ do
            x <- atomically $ readTChan chan
            return (x,chanTopic)
    adv chanTopic
    let put m = do
            mb <- m
            case mb of
                Nothing -> return ()
                Just a -> writeTChan chan a
    return $ fmap put t
    
interleaveT :: Topic IO (m ()) -> Topic IO (m ()) -> Topic IO (m ())
interleaveT t1 t2 = fmap (either id id) (t1 <+> t2)
    
leftT :: Monad m => Topic IO (m (Maybe (Either a b))) -> Topic IO (m (Maybe a))
leftT tab = fmap2 go tab
    where
    go (Just (Left a)) = Just a
    go _ = Nothing

rightT :: Monad m => Topic IO (m (Maybe (Either a b))) -> Topic IO (m (Maybe b))
rightT tab = fmap2 go tab
    where
    go (Just (Right b)) = Just b
    go _ = Nothing

fstT :: Monad m => Topic IO (m (Maybe (a,b))) -> Topic IO (m (Maybe a))
fstT = fmap3 fst

sndT :: Monad m => Topic IO (m (Maybe (a,b))) -> Topic IO (m (Maybe b))
sndT = fmap3 snd
    
instance Published Say where
    published = publishedROS $ \t -> runHandler (\(Say str) -> reportMessage str) t >> return ()

instance (Published a,Published b) => Published (a,b) where
    published t = do
        (ta,tb) <- liftIO $ fmap (fstT >< sndT) $ Topic.tee t
        ta' <- published ta
        tb' <- published tb
        return $ fmap (uncurry (>>)) $ Topic.bothNew ta' tb'

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
        (ta,tb) <- liftIO $ fmap (leftT >< rightT) $ Topic.tee tab
        ta' <- published ta
        tb' <- published tb
        return $ interleaveT ta' tb'

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
    controller :: a -> Node ()
    
instance {-# OVERLAPPABLE #-} Published b => Controller b where
    controller b = do
        go <- published $ Topic.topicRate defaultRate $ Topic.repeat (return $ Just b)
        _ <- runHandler atomically go
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



