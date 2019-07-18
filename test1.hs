-- {-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -F -pgmFrosypp #-}

module Main where

import Rosy
import Rosy.Controller.Core (Published(..),Subscribed(..),publishedEvent,subscribedEvent,publishedMemory,subscribedMemory)
import Data.Default.Generics (Default(..))
import Data.Typeable (Typeable(..))
import GHC.Generics (Generic(..))

--data Instructions = Instructions [Instruction]
--instance Default Instructions where
--    def = Instructions [RotateLeft,WalkForward,WalkForward,WalkBackward,RotateRight]
--
--data Instruction = RotateLeft | RotateRight | WalkForward | WalkBackward
--
--data Action = Start | End | RotateTo Orientation | WalkTo Position
--instance Default Action where
--    def = Start
--
--step :: Position -> Orientation -> Action -> Instructions -> Maybe (Instructions,Action)
--step p@(Position x y) th@(Orientation o) a is = case a of
--    Start -> Just (next p th is)
--    End -> Nothing
--    RotateTo (Orientation o') -> if abs (o'-o) <= 0.01 then Just (next p th is) else Nothing
--    WalkTo (Position x' y') -> if abs (x'-x) <= 0.1 && abs (y'-y) <= 0.1 then Just (next p th is) else Nothing
--
--next :: Position -> Orientation -> Instructions -> (Instructions,Action)
--next p o is = case is of
--    Instructions [] -> (is,End)
--    Instructions (i:is) -> (Instructions is,) $ case i of
--        RotateLeft -> RotateTo $ addOrientation o (pi/2)
--        RotateRight -> RotateTo $ addOrientation o (-pi/2)
--        WalkForward -> WalkTo $ addPosition p o 30
--        WalkBackward -> WalkTo $ addPosition p o (-30)
--
--addOrientation :: Orientation -> Double -> Orientation
--addOrientation (Orientation o1) o2 = Orientation $ rectAngle (o1 + o2)
--
--addPosition :: Position -> Orientation -> Double -> Position
--addPosition (Position x y) (Orientation o) d = Position (gridCoord $ x + d * cos o) (gridCoord $ y + d * sin o)
--
--gridCoord :: Double -> Double
--gridCoord cm = roundFloating (cm / 30) * 30
--
--act :: Position -> Orientation -> Action -> Velocity
--act p (Orientation o) a = case a of
--    RotateTo (Orientation o') -> Velocity 0 (o'-o)
--    WalkTo p' -> Velocity (signal $ magnitudeVec dist) (rectAngle o-o)
--        where
--        dist = distPos p' p
--        signal x = if angleVec dist >= 0 && angleVec dist <= pi then x else -x
--    otherwise -> Velocity 0 0
--
--magnitudeVec :: Position -> Double
--magnitudeVec (Position x y) = sqrt (x^2 + y^2)
--
--angleVec :: Position -> Double
--angleVec (Position x y) = atan2 y x
--
--distPos :: Position -> Position -> Position
--distPos (Position x1 y1) (Position x2 y2) = Position (x1-x2) (y1-y2)
--
--rectAngle :: Double -> Double
--rectAngle o = (roundFloating (o / (pi/2))) * (pi/2)
--
--main = simulate (step,act)

data Mode = On | Off

test :: Memory Mode -> Mode
test = undefined

main = simulate test
