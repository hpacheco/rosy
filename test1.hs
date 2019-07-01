{-# LANGUAGE ScopedTypeVariables #-}

module Main where
    
import Rosy.Interface
import Rosy

import Ros.Node
import Ros.Topic as Topic
import Ros.Nav_msgs.Odometry as Odometry
import Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
import Ros.Kobuki_msgs.Led as Led

import Debug.Trace

onLed :: Velocity -> Led1
onLed _ = Led1 Orange

onSound :: BumperCenter -> Sound
onSound (BumperCenter Pressed) = OnSound
onSound (BumperCenter Released) = OffSound

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

bumpleft :: BumperLeft -> Velocity
bumpleft _ = Velocity (-8) (pi/2)

bumpcenter :: BumperCenter -> Velocity
bumpcenter _ = Velocity (-8) (pi/2)

bumpright :: BumperRight -> Velocity
bumpright _ = Velocity (-8) (pi/2)

cliffleft :: CliffLeft -> Velocity
cliffleft _ = Velocity (-8) (pi/2)

cliffcenter :: CliffCenter -> Velocity
cliffcenter _ = Velocity (-8) (pi/2)

cliffright :: CliffRight -> Velocity
cliffright _ = Velocity (-8) (pi/2)

walk :: Orientation -> Velocity
walk (Orientation o) = (Velocity 8 orect)
    where
    orect = ceilingFloating (o / (pi/2)) * (pi/2)

--randomWalk :: (BumperLeft,BumperCenter,BumperRight,CliffLeft,CliffCenter,CliffRight)
--           -> (Velocity,Say)
--randomWalk ((BumperLeft Pressed,_,_,_,_,_))    = (Velocity 0 (-1),Say "bumpl")
--randomWalk ((_,BumperCenter Pressed,_,_,_,_))  = (Velocity 0 1,Say "bumpc")
--randomWalk ((_,_,BumperRight Pressed,_,_,_))   = (Velocity 0 1,Say "bumpr")
--randomWalk ((_,_,_,CliffLeft Cliff,_,_))       = (Velocity 0 (-1),Say "cliffl")
--randomWalk ((_,_,_,_,CliffCenter Cliff,_))     = (Velocity 0 1,Say "cliffc")
--randomWalk ((_,_,_,_,_,CliffRight Cliff))      = (Velocity 0 1,Say "cliffr")
--randomWalk _                                        = (Velocity 4 0,Say "walk")

main = simulate (bumpleft,bumpcenter,bumpright,cliffleft,cliffcenter,cliffright,walk)