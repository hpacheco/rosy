{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

module Prelude
  ( module Rosy
  , module General
  , Published(..), Subscribed(..), publishedEvent, subscribedEvent, publishedMemory, subscribedMemory
  , Default(..)
  , Typeable(..)
  , Generic(..)
  . Generic1(..)
  ) where

import "base" Prelude as General hiding ((>>=),(>>),return,fail)
import Rosy 
import Rosy.Controller.Core (Published(..),Subscribed(..),publishedEvent,subscribedEvent,publishedMemory,subscribedMemory)
import Data.Default.Generics (Default(..))
import Data.Typeable (Typeable(..))
import GHC.Generics (Generic(..),Generic1(..))
