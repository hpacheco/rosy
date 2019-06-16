module Main where
    
import Rosy.Interface
import Rosy

import Ros.Node
import Ros.Topic as Topic
import Ros.Nav_msgs.Odometry as Odometry
import Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
import Ros.Kobuki_msgs.Led as Led

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

randomWalk :: Maybe (BumperLeft,BumperCenter,BumperRight,CliffLeft,CliffCenter,CliffRight)
           -> Velocity
randomWalk (Just (BumperLeft Pressed,_,_,_,_,_))    = Velocity 0 (-1)
randomWalk (Just (_,BumperCenter Pressed,_,_,_,_))  = Velocity 0 1
randomWalk (Just (_,_,BumperRight Pressed,_,_,_))   = Velocity 0 1
randomWalk (Just (_,_,_,CliffLeft Cliff,_,_))       = Velocity 0 (-1)
randomWalk (Just (_,_,_,_,CliffCenter Cliff,_))     = Velocity 0 1
randomWalk (Just (_,_,_,_,_,CliffRight Cliff))      = Velocity 0 1
randomWalk _                                        = Velocity 1 0

main = simulate randomWalk