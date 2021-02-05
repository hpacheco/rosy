{-# LANGUAGE CPP, DeriveGeneric, TemplateHaskell #-}

module Rosy.Robot.Turtlesim.State where

import qualified Rosy.Controller.Turtlesim as Controller

import Ros.Node
import Ros.Rate
import Ros.Topic as Topic hiding (fst,snd,mapM)
import Ros.Topic.Util as Topic 
import Ros.Turtlesim.SetPenRequest
import Ros.Turtlesim.Pose as Pose
import Ros.Geometry_msgs.Twist as Twist
import Ros.Turtlesim.TeleportAbsoluteRequest 
import Ros.Turtlesim.TeleportAbsoluteResponse 
import Ros.Turtlesim.TeleportRelativeRequest 
import Ros.Turtlesim.TeleportRelativeResponse 
import Rosy.Util

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import Data.Typeable
import Data.List as List
import Data.Maybe
import Data.Default.Generics as D
import GHC.Generics as G
import GHC.Conc
import Lens.Family.TH
import Lens.Family (over,set)
import Data.DList as DList

#if defined(ghcjs_HOST_OS)
#else
import Graphics.Gloss.Juicy
#endif
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import Paths_rosy
import System.FilePath

data RobotState = RobotState
    { _robotVel       :: TVar (Twist,UTCTime) -- desired velocity
    , _robotPose      :: TVar Pose
    , _robotPen       :: TVar SetPenRequest 
    , _robotId        :: Int
    , _robotName      :: TVar String
    , _robotImage     :: Picture
    , _robotPath      :: TVar ([Picture],DList ((Float,Float),Int,Color))
    , _robotOn        :: TMVar ()
    } deriving (Typeable, G.Generic)

$(makeLenses ''RobotState)

findRobot :: String -> [RobotState] -> IO (Maybe RobotState)
findRobot name [] = return Nothing
findRobot name (r:rs) = do
    rname <- atomically $ readTVar $ _robotName r
    if rname==name
        then return $ Just r
        else findRobot name rs

findInactiveRobot :: [RobotState] -> IO (Maybe RobotState)
findInactiveRobot [] = return Nothing
findInactiveRobot (r:rs) = do
    off <- atomically $ isEmptyTMVar $ _robotOn r
    if off
        then return $ Just r
        else findInactiveRobot rs

loadTurtle :: String -> IO Picture
#if defined(ghcjs_HOST_OS)
loadTurtle str = loadSizedImageById 45 45 (takeBaseName str)
#else
loadTurtle str = do
    pic' <- getDataFileName $ "images" </> str
    Just img <- loadJuicyPNG pic'
    return img
#endif

newRobotState :: (Int,FilePath) -> IO RobotState
newRobotState (i,pic) = do
    img <- loadTurtle pic
    atomically $ do
        vel <- newTVar $ D.def
        pose <- newTVar $ Pose (250/45) (250/45) 0 0 0
        let (r,g,b,a) = rgbaOfColor penColor
        pen <- newTVar $ SetPenRequest (toEnum $ round $ r * 255) (toEnum $ round $ g * 255) (toEnum $ round $ b * 255) 3 0
        path <- newTVar ([],DList.empty)
        onoff <- if i==1 then newTMVar () else newEmptyTMVar
        name <- newTVar ("turtle"++show i)
        return $ RobotState vel pose pen i name img path onoff

resetRobotState :: RobotState -> IO ()
resetRobotState st = do
    atomically $ do
        writeTVar (_robotVel st) $ D.def
        writeTVar (_robotPose st) $ Pose (250/45) (250/45) 0 0 0
        let (r,g,b,a) = rgbaOfColor penColor
        writeTVar (_robotPen st) $ SetPenRequest (toEnum $ round $ r * 255) (toEnum $ round $ g * 255) (toEnum $ round $ b * 255) 3 0
        writeTVar (_robotPath st) ([],DList.empty)
        tryTakeTMVar (_robotOn st)
        when (_robotId st==1) $ putTMVar (_robotOn st) ()
        writeTVar (_robotName st) $ "turtle"++show (_robotId st)
    
killRobotState :: RobotState -> IO ()
killRobotState st = do
    atomically $ do
        writeTVar (_robotVel st) $ D.def
        writeTVar (_robotPose st) $ Pose (250/45) (250/45) 0 0 0
        let (r,g,b,a) = rgbaOfColor penColor
        writeTVar (_robotPen st) $ SetPenRequest (toEnum $ round $ r * 255) (toEnum $ round $ g * 255) (toEnum $ round $ b * 255) 3 0
        takeTMVar (_robotOn st)
        writeTVar (_robotName st) $ "turtle"++show (_robotId st)

spawnRobotState :: Pose -> String -> RobotState -> IO ()
spawnRobotState pose name st = do
    atomically $ do
        writeTVar (_robotPose st) pose
        putTMVar (_robotOn st) ()
        unless (List.null name) $ writeTVar (_robotName st) name

setPenRobotState :: SetPenRequest -> RobotState -> IO ()
setPenRobotState req st = do
    atomically $ do
        writeTVar (_robotPen st) req
        
teleportAbsoluteRobotState :: TeleportAbsoluteRequest -> RobotState -> IO ()
teleportAbsoluteRobotState (TeleportAbsoluteRequest x y o) st = do
    atomically $ do
        modifyTVar (_robotPose st) $ \pose -> pose { Pose._x = truncateCoord x, Pose._y = truncateCoord y, Pose._theta = o }
        
teleportRelativeRobotState :: TeleportRelativeRequest -> RobotState -> IO ()
teleportRelativeRobotState (TeleportRelativeRequest vlin' vrot') st = do
    atomically $ do
        modifyTVar (_robotPose st) $ \pose -> 
            let x = Pose._x pose in
            let y = Pose._y pose in
            let rads = Pose._theta pose in
            let rads' = rads + vrot' in
            let v' = scalarVec vlin' rads' in
            let (x',y') = (x,y) `addVec` v' in
            let x'' = truncateCoord x' in
            let y'' = truncateCoord y' in
            pose { Pose._x = x'', Pose._y = y'', Pose._theta = rads' }

backgroundColor :: Color
backgroundColor = makeColorI 69 86 255 255

penColor :: Color
penColor = makeColorI 179 184 255 255

truncateCoord :: Float -> Float
truncateCoord c = if c < 0 then 0 else if c > 500/45 then 500/45 else c

robotImages :: [FilePath]
robotImages =  ["box-turtle.png","robot-turtle.png","sea-turtle.png","diamondback.png","electric.png","fuerte.png","groovy.png","hydro.png","indigo.png"]

newRobotStates :: IO [RobotState]
newRobotStates = mapM newRobotState (zip [1..] robotImages)

resetRobotStates :: [RobotState] -> IO ()
resetRobotStates sts = Control.Monad.mapM_ resetRobotState sts

debug :: String -> IO ()
debug msg = putStrLn msg



