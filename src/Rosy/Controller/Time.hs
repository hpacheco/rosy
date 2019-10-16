{-# LANGUAGE TypeSynonymInstances, ViewPatterns, PatternSynonyms #-}

module Rosy.Controller.Time
    ( Seconds(..), Time(..), pattern Seconds, pattern Clock, Clock(..)
    , doubleToSeconds
    , hours, minutes, seconds, milliseconds
    ) where

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Rosy.Util
import Rosy.Controller.Core

import Ros.Rate
import qualified Ros.Topic as Topic
import qualified Ros.Topic.Util as Topic

import Control.Monad.Trans
import Control.Monad

doubleToSeconds :: Double -> Seconds
doubleToSeconds = realToFrac

type Clock = UTCTime

-- | The current time in hours, minutes, seconds and milliseconds.
pattern Clock :: Int -> Int -> Int -> Int -> UTCTime
pattern Clock h m s ms <- (clockFromUTCTime -> (h,m,s,ms))

type Seconds = POSIXTime

-- | The time in seconds since since 1970-01-01 00:00 UTC
pattern Seconds :: Seconds -> UTCTime
pattern Seconds s <- (utcTimeToPOSIXSeconds -> s)
    where
    Seconds s = posixSecondsToUTCTime s

type Time = UTCTime

hours :: Time -> Int
hours = fst4 . clockFromUTCTime

minutes :: Time -> Int
minutes = snd4 . clockFromUTCTime

seconds :: Time -> Int
seconds = thr4 . clockFromUTCTime

milliseconds :: Time -> Int
milliseconds = fou4 . clockFromUTCTime

clockFromUTCTime :: UTCTime -> (Int,Int,Int,Int)
clockFromUTCTime utc = (h,m,s,ms)
    where
    diff = utctDayTime utc
    h = remBy 24 $ quotBy 3600 diff
    m = remBy 60 $ quotBy 60 diff
    s = remBy 60 $ quotBy 1 diff
    ms = floor $ diff - realToFrac (floor diff) * 1000

instance Subscribed UTCTime where
    subscribed = return $ Topic.topicRate defaultRate $ Topic.repeatM $ liftM return $ lift getCurrentTime
    
instance Subscribed Seconds where
    subscribed = return $ Topic.topicRate defaultRate $ Topic.repeatM $ liftM (return . utcTimeToPOSIXSeconds) $ lift getCurrentTime

