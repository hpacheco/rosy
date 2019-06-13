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

main = simulate $ onSound