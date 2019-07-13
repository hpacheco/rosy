{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -F -pgmFrosypp #-}

module Main where
    
--import qualified GHC.Generics as G
--import Data.Typeable
--    
--import Rosy.Controller.Core
--import Rosy.Interface
--import Rosy
--
--import Ros.Node
--import Ros.Topic as Topic
--import Ros.Nav_msgs.Odometry as Odometry
--import Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
--import Ros.Kobuki_msgs.Led as Led
--
--import qualified Data.Default.Generics as D
--
--import Debug.Trace

import Rosy
import Rosy.Controller.Core (Published(..),Subscribed(..),publishedMemory,subscribedMemory)
import Data.Default.Generics (Default(..))
import Data.Typeable (Typeable(..))
import GHC.Generics (Generic(..))

--onLed :: Velocity -> Led1
--onLed _ = Led1 Orange
--
--onSound :: BumperCenter -> Sound
--onSound (BumperCenter Pressed) = OnSound
--onSound (BumperCenter Released) = OffSound

--printOdom1 :: Odometry -> IO ()
--printOdom1 o = putStrLn $ "1 " ++ show (TwistWithCovariance._twist $ Odometry._twist o)
--
--mkLed1 :: Topic IO Odometry -> Topic IO Led
--mkLed1 om = fmap (\o -> Led value_ORANGE) om'
--    where
--    effect o = do
--        liftIO $ putStrLn $  "odom " ++ show (TwistWithCovariance._twist $ Odometry._twist o)
--        return o
--    om' = Topic.mapM effect om

--randomWalk :: Maybe (BumperLeft,BumperCenter,BumperRight,CliffLeft,CliffCenter,CliffRight)
--           -> (Velocity,Say)
--randomWalk (Just (BumperLeft Pressed,_,_,_,_,_))    = (Velocity 0 (-1),Say "bumpl")
--randomWalk (Just (_,BumperCenter Pressed,_,_,_,_))  = (Velocity 0 1,Say "bumpc")
--randomWalk (Just (_,_,BumperRight Pressed,_,_,_))   = (Velocity 0 1,Say "bumpr")
--randomWalk (Just (_,_,_,CliffLeft Cliff,_,_))       = (Velocity 0 (-1),Say "cliffl")
--randomWalk (Just (_,_,_,_,CliffCenter Cliff,_))     = (Velocity 0 1,Say "cliffc")
--randomWalk (Just (_,_,_,_,_,CliffRight Cliff))      = (Velocity 0 1,Say "cliffr")
--randomWalk _                                        = (Velocity 4 0,Say "walk")

--    deriving (Typeable,G.Generic)
--
--instance Subscribed Mode where
--    subscribed = subscribedMemory
--    
--instance Published Mode where
--    published = publishedMemory
--    
--instance D.Default Mode
--
--data Mode = Ok | Panic Clock
--
---- | the controller is in panic mode during 1 second since the last emergency
--mode :: Mode -> Clock -> Mode
--mode (Panic old) new = if seconds new-seconds old > 1 then Ok else Panic old
--mode Ok _ = Ok 
--
---- | when the robot has a serious event, signal an emergency
--emergency :: Either Bumper Cliff -> Clock -> Mode
--emergency _ now = Panic now
--
---- | move the robot depending on the mode
--walk :: Orientation -> Mode -> Velocity
--walk (Orientation o) Ok = Velocity 5 0
--walk (Orientation o) (Panic _) = Velocity 0 (pi/2)
--
--main = simulate (emergency,mode,walk)

--    deriving (Typeable,G.Generic)
--
--instance Subscribed Blink where
--    subscribed = subscribedMemory
--    
--instance Published Blink where
--    published = publishedMemory
--    
--instance D.Default Blink

--data Blink = Off | On
--
--blink :: Blink -> (Led1,Blink)
--blink Off = (Led1 Black,On)
--blink On = (Led1 Red,Off)
--
--main = simulate blink

--data Hit = NoHit | Hit
--warningWall :: BumperCenter -> Maybe Hit
--warningWall (BumperCenter Pressed)  = Just Hit
--warningWall (BumperCenter Released) = Nothing
--
--playError :: Hit -> Maybe Sound
--playError Hit = Just OnSound
--playError NoHit = Nothing
--
--accelerate :: Velocity -> Velocity
--accelerate (Velocity linear angular) = Velocity (linear+1) angular
--
--main = simulate (accelerate,warningWall,playError)



--moveTo :: Position -> (Position,Orientation,Velocity) -> (Velocity,Say)
--moveTo dest (src,Orientation angle,vel) = (vel',Say $ show vel')
--    where
--    distance = subPos dest src
--    vrot' = anglePos distance - angle
--    vlin' = if vrot' < 0.1 then magnitudePos distance else 0
--    vel' = subVel (Velocity vlin' vrot') vel
--
--subPos :: Position -> Position -> Position
--subPos (Position x1 y1) (Position x2 y2) = Position (x1-x2) (y1-y2)
--
--subVel :: Velocity -> Velocity -> Velocity
--subVel (Velocity x1 y1) (Velocity x2 y2) = Velocity (x1-x2) (y1-y2)
--
--magnitudePos :: Position -> Double
--magnitudePos (Position x y) = sqrt (x^2 + y^2)
--
--anglePos :: Position -> Double
--anglePos (Position x y) = atan2 y x
--
--arrived (Position x1 y1) (Position x2 y2) = abs (x1-x2) < 0.1 && abs (y1-y2) < 0.1
--
--data Path = Path [Position]
--
--movePath :: Path -> (Position,Orientation,Velocity) -> ((Velocity,Say),Path)
--movePath (Path []) (p,o,v) = movePath path (p,o,v)
--movePath (Path (d:ds)) (p,o,v) = if arrived d p
--  then movePath (Path ds) (p,o,v)
--  else (moveTo d (p,o,v),Path (d:ds))
--
--path = Path [Position (-60) 100,Position 70 80,Position 70 (-80),Position (-10) (-80)]
--
--main = simulate (movePath)


data Order = Move Velocity
instance Default Order where
   def = Move (Velocity 8 0)

ouch :: Velocity -> Bumper -> Maybe (Order,Say)
ouch v (Bumper _ Pressed) = Just (Move (negVel v),Say "ouch!!")
ouch v _ = Nothing

negVel (Velocity vlin vrot) = Velocity (-vlin) (-vrot)

order :: Order -> Velocity
order (Move v) = v

main = simulate (order,ouch)

