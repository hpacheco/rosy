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

data Emergency = Emergency Clock
data Mode = Panic | Ok

-- | the controller is in panic mode during 1 second since the last emergency
mode :: Maybe Emergency -> Clock -> Maybe Mode -> (Mode,Say)
mode (Just (Emergency old)) new _ = if seconds new-seconds old > 1 then (Ok,Say $ "ok " ++ show old) else (Panic,Say $ "panic " ++ show old)
mode Nothing new Nothing = (Ok,Say "ok")
mode Nothing new (Just mode) = (mode,Say "same")

-- | when the robot has a serious event, signal an emergency
emergency :: Either Bumper Cliff -> Clock -> Emergency
emergency _ now = Emergency now

-- | move the robot depending on the mode
walk :: Orientation -> Mode -> Velocity
walk (Orientation o) Ok = Velocity 5 (roundFloating (o / (pi/2)) * (pi/2))
walk (Orientation o) Panic = Velocity 0 (pi/2)

main = simulate (emergency,mode,walk)


