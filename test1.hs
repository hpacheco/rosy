{-# LANGUAGE FlexibleContexts, UndecidableInstances, TupleSections, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -F -pgmFrosypp #-}

module Main where

import Rosy
import Rosy.Controller.Core (Published(..),Subscribed(..),publishedMemory,subscribedMemory)
import Data.Default.Generics (Default(..))
import Data.Typeable (Typeable(..))
import GHC.Generics (Generic(..))

led1 :: Velocity -> (Led1,Led2)
led1 (Velocity x y) = let r1 = if x > 1 then Led1 Red else Led1 Green
                          r2 = if y > 1 then Led2 Red else Led2 Green in
                      (r1,r2)

go :: Velocity 
go = Velocity 20 2

main = simulate (led1,go)

