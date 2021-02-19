{-# LANGUAGE ScopedTypeVariables, TupleSections, UndecidableInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types, TypeFamilyDependencies, TypeInType, PolyKinds, TypeOperators, TypeFamilies, FlexibleContexts, GADTs #-}

module Rosy.Interface.Task.Types where

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

--import Data.Type.Bool
--import Data.Type.Equality
--import Data.Type.Set hiding (Proxy(..))
--import Type.Compare
--import qualified Control.Effect as E

import Graphics.Gloss

import Ros.Node
import Rosy.Controller.Core
--import Rosy.Robot.Kobuki.State as Kobuki
--import Rosy.Viewer.Kobuki.State as Kobuki
--import Rosy.Robot.Kobuki.Core as Kobuki
--import Rosy.Viewer.Kobuki.Core as Kobuki
--import Rosy.Robot.Turtlesim.State as Turtle
--import Rosy.Viewer.Turtlesim.State as Turtle
--import Rosy.Robot.Turtlesim.Core as Turtle
--import Rosy.Viewer.Turtlesim.Core as Turtle
--import Rosy.Interface

import Ros.Topic.Util as Topic
import Ros.Topic as Topic

import Unsafe.Coerce
import System.IO.Unsafe
import GHC.Conc

-- | A 'Task' is a monadic 'Effect' that can be composed sequentially, in the sense that a task will only start after the previous task has finished.
data Task feed end where
    Task :: (Typeable feed,Typeable end,Published init,Controller action) => init -> action -> Task feed end
    RetTask :: a -> Task f a
    BindTask :: Task fa a -> (a -> Task fa b) -> Task fa b
    SubTask :: (Typeable f1) => (f1 -> Maybe f2) -> Task f1 a -> Task f2 a
    CoreTask :: Node a -> Task f a
    
-- | The type of 'Task' 'call's inside controllers.
data Call where
    Call :: (Subscribed when,Published see,Published res,Typeable when,Typeable see,Typeable res) => Task feed end -> (when -> Maybe Cancel) -> (feed -> see) -> (end -> res) -> Call 
    
instance Monad (Task feed) where
    return = RetTask
    (>>=) = BindTask
   
subTask :: (Typeable f1) => (f1 -> Maybe f2) -> Task f1 a -> Task f2 a 
subTask f m = SubTask f m
    
instance Functor (Task f) where
    fmap f m = m >>= return . f
instance Applicative (Task f) where
    pure = return
    m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

-- | A 'Task' finishing event.
data Done a = Done {unDone :: a }
  deriving (Show,Eq,Ord,Typeable)
instance Typeable a => Subscribed (Done a) where
    subscribed = subscribedEvent
instance Typeable a => Published (Done a) where
    published = publishedEvent

-- | A 'Task' cancelling event.
data Cancel = Cancel
  deriving (Show,Eq,Ord,Typeable)
instance Subscribed Cancel where
    subscribed = subscribedEvent
instance Published Cancel where
    published = publishedEvent
  
-- | A type-level tag that allows a 'Task' controller to publish feed messages.
data Feedback a = Feedback { unFeedback :: a }
  deriving (Show,Eq,Ord,Typeable)
instance Typeable a => Subscribed (Feedback a) where
    subscribed = subscribedEvent
instance Typeable a => Published (Feedback a) where
    published = publishedEvent







