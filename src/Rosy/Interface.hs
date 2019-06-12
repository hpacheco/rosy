{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

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

import Ros.Topic.Util as Topic

startNode :: Node () -> WorldState -> IO ()
startNode n w = runNode "rosy-simulator" $ do
    n
    runRobotNodes w
    runViewerNodes w

class Nodlet a where
    nodlet :: a -> Node ()

instance Published b => Nodlet b where
    nodlet b = published $ Topic.topicRate 1 $ Topic.repeat b
    
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
    
instance (Subscribed a,Published b) => Published (a -> b) where
    published tab = do
        ta <- subscribed
        published (fmap (uncurry ($)) $ tab `Topic.everyNew` ta)

simulate :: Nodlet a => a -> IO ()
simulate n = do
    w <- newWorldState
    concurrently_ (startNode (nodlet n) w) (runViewer w)