{-# LANGUAGE TupleSections, CPP, DeriveGeneric, ScopedTypeVariables, ViewPatterns #-}

module Rosy.Robot.Turtlesim.Core where

import Rosy.Robot.Turtlesim.State
import Rosy.Viewer.Turtlesim.State
import qualified Rosy.Controller.Core as Controller
import qualified Rosy.Controller.Turtlesim as Controller
import Rosy.Controller.Core
import Rosy.Util

import Ros.Node
import Ros.Rate
import Ros.Topic as Topic hiding (fst,snd,forever)
import Ros.Topic.Util as Topic 
--import Ros.Kobuki_msgs.Led as Led
--import Ros.Kobuki_msgs.Sound as Sound
--import Ros.Kobuki_msgs.BumperEvent as BumperEvent
--import Ros.Kobuki_msgs.ButtonEvent as ButtonEvent
--import Ros.Kobuki_msgs.CliffEvent as CliffEvent
--import Ros.Kobuki_msgs.WheelDropEvent as WheelDropEvent
--import Ros.Kobuki_msgs.RobotStateEvent as RobotStateEvent
--import Ros.Nav_msgs.Odometry as Odometry
import Ros.Geometry_msgs.Twist as Twist
--import Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
--import Ros.Geometry_msgs.Pose as Pose
--import Ros.Geometry_msgs.PoseWithCovariance as PoseWithCovariance
import Ros.Std_srvs.EmptyRequest
import Ros.Std_srvs.EmptyResponse
import Ros.Turtlesim.KillRequest
import Ros.Turtlesim.KillResponse
import Ros.Turtlesim.SpawnRequest
import Ros.Turtlesim.SpawnResponse
import Ros.Geometry_msgs.Vector3 as Vector3
--import Ros.Geometry_msgs.Point as Point
import Ros.Turtlesim.Pose as Pose
import Ros.Turtlesim.SetPenRequest as Pen
import Ros.Turtlesim.SetPenResponse as Pen
import Ros.Turtlesim.TeleportAbsoluteRequest 
import Ros.Turtlesim.TeleportAbsoluteResponse 
import Ros.Turtlesim.TeleportRelativeRequest 
import Ros.Turtlesim.TeleportRelativeResponse 

import Control.Concurrent.STM
import Control.Monad as Monad
import Control.Monad.Trans
import Data.Typeable
import Data.Time.Clock
import Data.Word as Word
import Data.Default.Generics as D
import GHC.Generics as G
import GHC.Conc
import Safe
import Prelude as P
import Data.Maybe as Maybe
import Data.List as List
import Data.DList as DList

import Lens.Family (over,set)

import System.FilePath

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Data.Color
    
import Control.Exception

import Paths_rosy


#if defined(ghcjs_HOST_OS)
#else
import System.Process
#endif

-- ** Robot inputs

readRobotVelocity :: RobotState -> Node ThreadId
readRobotVelocity st = do
    v <- subscribe ("turtle"++show (_robotId st) </> "cmd_vel")
    flip runHandler v $ \twist -> liftIO $ atomically $ do
        now <- unsafeIOToSTM $ getCurrentTime
        writeTVar (_robotVel st) (twist,now)

-- turtlesim core applies the desired velocity during 1s, and zeroes them afterwards
updateRobotVelocity :: RobotState -> STM Twist
updateRobotVelocity st = do
    (vel,since) <- readTVar (_robotVel st)
    now <- unsafeIOToSTM getCurrentTime
    let diff = diffUTCTime now since
    if diff >= 1
        then writeTVar (_robotVel st) (D.def,now) >> return D.def
        else return vel

-- ** Physics Engine

runRobotPhysics :: RobotState -> Node ThreadId
runRobotPhysics st = liftIO $ do
    go <- rateLimiter robotFrequency $ atomically $ do
        readTMVar (_robotOn st)
        p <- readTVar (_robotPose st)
        let px = Pose._x p
        let py = Pose._y p
        let rads = Pose._theta p
        
        -- update position
        vel' <- updateRobotVelocity st
        let vlin' = realToFrac $ Vector3._x $ Twist._linear vel'
        let vrot' = realToFrac $ Vector3._z $ Twist._angular vel'
        let rads' = rads + vrot' / realToFrac robotFrequency
        let v' = scalarVec vlin' rads'
        let (px',py') = (px,py) `addVec` (v' `divVec` realToFrac robotFrequency) 
        let px'' = truncateCoord px'
        let py'' = truncateCoord py'
        
        writeTVar (_robotPose st) $ Pose px'' py'' rads' (vlin') (vrot')
        
        pen <- readTVar (_robotPen st)
        when (Pen._off pen == 0) $ do
            let w = fromEnum $ Pen._width pen
            let c = makeColorI (fromEnum $ Pen._r pen) (fromEnum $ Pen._g pen) (fromEnum $ Pen._b pen) 255
            modifyTVar (_robotPath st) $ \(pics,xs) -> (pics,DList.snoc xs ((px'',py''),w,c) )
                
    forkIO $ forever go

-- ** Robot Outputs
    
writeRobotPose :: RobotState -> Node ()
writeRobotPose st = do
    advertise ("turtle" ++ show (_robotId st) </> "pose") $ topicRate defaultRate $ repeatM $ liftTIO $ atomically $ readTVar (_robotPose st)
  where
    liftTIO :: IO a -> TIO a
    liftTIO = liftIO

clearService :: WorldState -> EmptyRequest -> Node EmptyResponse
clearService w _ = do
    (r::Int) <- liftM (maybe 69 id) $ getParameter "background_r"
    (g::Int) <- liftM (maybe 86 id) $ getParameter "background_g"
    (b::Int) <- liftM (maybe 255 id) $ getParameter "background_b"
    liftIO $ atomically $ modifyTVar (_worldStateBackground w) $ const (makeColorI r g b 255)
    forM_ (_worldStateRobots w) $ \(st::RobotState) -> do
        liftIO $ atomically $ writeTVar (_robotPath st) ([],DList.empty)
    return EmptyResponse

resetService :: WorldState -> EmptyRequest -> Node EmptyResponse
resetService w _ = do
    (r::Int) <- liftM (maybe 69 id) $ getParameter "background_r"
    (g::Int) <- liftM (maybe 86 id) $ getParameter "background_g"
    (b::Int) <- liftM (maybe 255 id) $ getParameter "background_b"
    liftIO $ atomically $ modifyTVar (_worldStateBackground w) $ const (makeColorI r g b 255)
    forM_ (_worldStateRobots w) $ \(st::RobotState) -> do
        liftIO $ resetRobotState st
    return EmptyResponse

killService :: WorldState -> KillRequest -> Node KillResponse
killService w (KillRequest name) = do
    mb <- liftIO $ findRobot name (_worldStateRobots w)
    case mb of 
        Nothing -> return KillResponse
        Just r -> do
            liftIO $ killRobotState r
            return KillResponse

spawnService :: WorldState -> SpawnRequest -> Node SpawnResponse
spawnService w req@(SpawnRequest x y o name) = do
    mb <- liftIO $ findInactiveRobot (_worldStateRobots w)
    case mb of 
        Nothing -> return $ SpawnResponse ""
        Just r -> do
            liftIO $ spawnRobotState (Pose x y o 0 0) name r
            return $ SpawnResponse name

setPenService :: WorldState -> Int -> SetPenRequest -> Node SetPenResponse
setPenService w i req@(SetPenRequest r g b width off)
    | i >= 1 && i <= 9 = do
        let r = (_worldStateRobots w) !! (i-1)
        liftIO $ setPenRobotState req r
        return $ SetPenResponse
    | otherwise = return SetPenResponse

teleportAbsoluteService :: WorldState -> Int -> TeleportAbsoluteRequest -> Node TeleportAbsoluteResponse
teleportAbsoluteService w i req@(TeleportAbsoluteRequest x y o)
    | i >= 1 && i <= 9 = do
        let r = (_worldStateRobots w) !! (i-1)
        liftIO $ teleportAbsoluteRobotState req r
        return $ TeleportAbsoluteResponse
    | otherwise = return TeleportAbsoluteResponse

teleportRelativeService :: WorldState -> Int -> TeleportRelativeRequest -> Node TeleportRelativeResponse
teleportRelativeService w i req@(TeleportRelativeRequest lin ang)
    | i >= 1 && i <= 9 = do
        let r = (_worldStateRobots w) !! (i-1)
        liftIO $ teleportRelativeRobotState req r
        return $ TeleportRelativeResponse
    | otherwise = return TeleportRelativeResponse

runRobotNodes :: WorldState -> Node ()
runRobotNodes w = do
    -- register params
    setParameter "background_r" (69::Int)
    setParameter "background_g" (86::Int)
    setParameter "background_b" (255::Int)
    -- register services
    registerService "clear" (clearService w)
    registerService "reset" (resetService w)
    registerService "kill" (killService w)
    registerService "spawn" (spawnService w)
    forM [1..9] $ \i -> do
        registerService ("turtle"++show i </> "set_pen") (setPenService w i)
        registerService ("turtle"++show i </> "teleport_absolute") (teleportAbsoluteService w i)
        registerService ("turtle"++show i </> "teleport_relative") (teleportRelativeService w i)
    -- spin
    let sts = _worldStateRobots w
    t3s <- forM sts $ readRobotVelocity
    t4s <- forM sts $ runRobotPhysics
    forM sts $ writeRobotPose
    let ts = t3s++t4s
    addCleanup $ P.mapM_ killThread ts
    
-- frequency of the physics engine (Hz)
robotFrequency :: Double
robotFrequency = 15

