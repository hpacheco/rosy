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

-- * User Events

data UserEvent = UserEvent
    { userEventChan :: Chan ()
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
            c <- newChan
            let stream = Topic $ do { x <- readChan c; return (x, stream) }
            stream' <- liftIO $ share stream
            let e = UserEvent c stream'
            return (Map.insert ty e events,e)

-- A general instance for publishing user-defined events
instance {-# OVERLAPPABLE #-} Typeable a => Published a where
    published = publishedType undefined

publishedType :: Typeable a => a -> Topic IO a -> Node ()
publishedType ty topic = do
        e <- liftIO $ getUserEvent (typeOf ty)
        flip runHandler (fmap unsafeCoerce topic) $ \a -> writeChan (userEventChan e) a
        return ()
    
-- A general insstance for subscribing user-defined events
instance {-# OVERLAPPABLE #-} Typeable a => Subscribed a where
    subscribed = subscribedType undefined

subscribedType :: Typeable a => a -> Node (Topic IO a)
subscribedType ty = do
        e <- liftIO $ getUserEvent (typeOf ty)
        return $ fmap unsafeCoerce (userEventTopic e)

-- * Controllers

startNode :: Node () -> WorldState -> IO ()
startNode n w = runNode "rosy-simulator" $ do
    n
    runRobotNodes w
    runViewerNodes w

class Controller a where
    controller :: a -> Node ()

instance Published b => Controller b where
    controller b = published $ Topic.topicRate 10 $ Topic.repeat b

instance Controller a => Controller [a] where
    controller = mapM_ controller
    
--instance (Nodlet a,Nodlet b) => Nodlet (a,b) where
--    nodlet (a,b) = nodlet a >> nodlet b
--
--instance (Nodlet a,Nodlet b,Nodlet c) => Nodlet (a,b,c) where
--    nodlet (a,b,c) = nodlet a >> nodlet b >> nodlet c
--
--instance (Nodlet a,Nodlet b,Nodlet c,Nodlet d) => Nodlet (a,b,c,d) where
--    nodlet (a,b,c,d) = nodlet a >> nodlet b >> nodlet c >> nodlet d
--
--instance (Nodlet a,Nodlet b,Nodlet c,Nodlet d,Nodlet e) => Nodlet (a,b,c,d,e) where
--    nodlet (a,b,c,d,e) = nodlet a >> nodlet b >> nodlet c >> nodlet d >> nodlet e
    
-- The general instance to lift functions to controllers
instance (Subscribed a,Published b) => Published (a -> b) where
    published tab = do
        ta <- subscribed
        published fmap (uncurry ($)) $ tab `Topic.bothNew` ta

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