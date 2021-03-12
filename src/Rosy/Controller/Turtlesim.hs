{-# LANGUAGE GADTs, ViewPatterns, OverloadedStrings, DeriveDataTypeable, DeriveGeneric, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures, DataKinds, Rank2Types #-}

module Rosy.Controller.Turtlesim where

import Data.Word as Word
import Data.Fixed
import Data.List as List
import Data.Typeable
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Lens.Family.TH (makeLensesBy)
import Lens.Family (view, set)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad.Trans
import Control.Monad
--import qualified Control.Effect as E

import Rosy.Controller.Core
import Rosy.Util
import Rosy.Interface.Task.Types

import Ros.Node
import Ros.Node.Type
import Ros.Topic as Topic
import qualified Ros.Topic.Util as Topic
import Ros.Topic.Util (TIO,(<+>)) 
import Ros.Turtlesim.Pose as Pose
--import Ros.Turtlesim.Color as Color

import Ros.Std_srvs.EmptyRequest
import Ros.Std_srvs.EmptyResponse
import Ros.Turtlesim.KillRequest
import Ros.Turtlesim.KillResponse
import Ros.Turtlesim.SpawnRequest
import Ros.Turtlesim.SpawnResponse as SpawnResponse
import Ros.Turtlesim.SetPenRequest
import Ros.Turtlesim.SetPenResponse
import Ros.Turtlesim.TeleportAbsoluteRequest 
import Ros.Turtlesim.TeleportAbsoluteResponse 
import Ros.Turtlesim.TeleportRelativeRequest 
import Ros.Turtlesim.TeleportRelativeResponse 

import System.FilePath

import GHC.TypeNats
import GHC.Natural

turtleString' :: IsTurtleNumber n => Proxy n -> String
turtleString' n = "turtle"++ show (natVal n)

turtleString :: Int -> String
turtleString n = "turtle"++ show n

invTurtleString :: String -> TurtleNumber
invTurtleString n | List.isPrefixOf "turtle" n = read (Prelude.drop 6 n)
                  | otherwise = 0

-- * Numbered turtles

class KnownNat n => IsTurtleNumber (n :: Nat) where
    
instance IsTurtleNumber 1 where
instance IsTurtleNumber 2 where
instance IsTurtleNumber 3 where    
instance IsTurtleNumber 4 where
instance IsTurtleNumber 5 where
instance IsTurtleNumber 6 where
instance IsTurtleNumber 7 where
instance IsTurtleNumber 8 where
instance IsTurtleNumber 9 where

-- | A property of a numbered turtle
data Turtle (n :: Nat) a = Turtle { unTurtle :: a }
    deriving (Show, Eq, Ord, Typeable, G.Generic)

instance D.Default a => D.Default (Turtle n a)

type TurtleNumber = Int

turtleNumber :: IsTurtleNumber n => Turtle n a -> TurtleNumber
turtleNumber (_::Turtle n a) = fromEnum $ natVal (Proxy::Proxy n)

turtleNumberProxy :: Turtle n a -> Proxy n
turtleNumberProxy _ = Proxy

data SomeTurtle a where
    SomeTurtle :: IsTurtleNumber n => Turtle n a -> SomeTurtle a
    
instance Functor SomeTurtle where
    fmap (f::a->b) (SomeTurtle (Turtle a::Turtle n a)) = SomeTurtle (Turtle (f a)::Turtle n b)

someTurtleNumber :: TurtleNumber -> SomeTurtle ()
someTurtleNumber 1 = SomeTurtle (Turtle ()::Turtle 1 ())
someTurtleNumber 2 = SomeTurtle (Turtle ()::Turtle 2 ())
someTurtleNumber 3 = SomeTurtle (Turtle ()::Turtle 3 ())
someTurtleNumber 4 = SomeTurtle (Turtle ()::Turtle 4 ())
someTurtleNumber 5 = SomeTurtle (Turtle ()::Turtle 5 ())
someTurtleNumber 6 = SomeTurtle (Turtle ()::Turtle 6 ())
someTurtleNumber 7 = SomeTurtle (Turtle ()::Turtle 7 ())
someTurtleNumber 8 = SomeTurtle (Turtle ()::Turtle 8 ())
someTurtleNumber 9 = SomeTurtle (Turtle ()::Turtle 9 ())

someTurtle :: TurtleNumber -> a -> SomeTurtle a
someTurtle n a = fmap (const a) (someTurtleNumber n)

onTurtle :: TurtleNumber -> (forall n. IsTurtleNumber n => Turtle n () -> res) -> res
onTurtle (someTurtleNumber -> SomeTurtle turtle) f = f turtle

instance Show a => Show (SomeTurtle a) where
    show (SomeTurtle t) = show t
instance Eq a => Eq (SomeTurtle a) where
    (SomeTurtle t1) == (SomeTurtle t2) = case sameNat (turtleNumberProxy t1) (turtleNumberProxy t2) of
        Nothing -> False
        Just _ -> unTurtle t1 == unTurtle t2
instance Ord a => Ord (SomeTurtle a) where
    compare (SomeTurtle t1) (SomeTurtle t2) = compare (natVal $ turtleNumberProxy t1,unTurtle t1) (natVal $ turtleNumberProxy t2,unTurtle t2) 

deriving instance Typeable (SomeTurtle a)

instance D.Default a => D.Default (SomeTurtle a) where
    def = SomeTurtle (Turtle D.def :: Turtle 1 a)

data AnyTurtle a = AnyTurtle { anyTurtleNumber :: TurtleNumber, unAnyTurtle :: a }
    deriving (Show, Eq, Ord, Typeable, G.Generic)

instance D.Default a => D.Default (AnyTurtle a) where
    def = AnyTurtle 1 D.def

-- * Turtlesim Subscriptions (give orders to the robot)

-- ** Velocity

instance IsTurtleNumber n => Published (Turtle n Velocity) where
    publishedProxy (Proxy :: Proxy (Turtle n Velocity)) = publishedROS (advertise (turtleString' (Proxy::Proxy n) </> "cmd_vel") . fmap (velocityToROS . unTurtle))
    
interleaves :: [Topic TIO a] -> Topic TIO a
interleaves [] = haltTopic
interleaves [t] = t
interleaves (t:ts) = fmap (either id id) (t <+> interleaves ts)
    
instance Published (AnyTurtle Velocity) where
    published t = do
        (Map.fromList -> chans,pubs) <- liftM unzip $ forM [1..9] $ \i -> do
            chan :: Chan Velocity <- liftIO $ newChan
            let chanTopic = Topic $ do
                    x <- lift $ readChan chan
                    return (return $ Just x,chanTopic)
            pub <- publishedROS (advertise ("turtle"++show i </> "cmd_vel") . fmap velocityToROS) chanTopic
            return ((i,chan),pub)
        lift $ flip runHandler t $ \smt -> liftIO $ do
            mb <- atomically smt
            case mb of
                Nothing -> return ()
                Just (AnyTurtle i v) -> do
                    case Map.lookup i chans of
                        Nothing -> return ()
                        Just chan -> liftIO $ writeChan chan v
        return $ interleaves pubs

-- * Turtlesim publications (see the robot's state)

-- ** Pose

instance IsTurtleNumber n => Subscribed (Turtle n Pose) where
    subscribedProxy (Proxy :: Proxy (Turtle n Pose)) = subscribedROS $ do
        pose <- subscribeBuffered 1 (turtleString' (Proxy::Proxy n) </> "pose")
        return $ fmap Turtle pose

instance Subscribed (AnyTurtle Pose) where
    subscribed = do
        chan <- liftIO $ newChan
        lift $ forM [1..9] $ \i -> do
            t <- subscribeBuffered 1 ("turtle" ++ show i </> "pose")
            flip runHandler_ t $ \v -> lift $ writeChan chan $ AnyTurtle i v
        let chanTopic = Topic $ do
                x <- lift $ readChan chan
                return (return x,chanTopic)
        return $ chanTopic

posePosition :: Pose -> Position
posePosition p = Position (realToFrac $ Pose._x p) (realToFrac $ Pose._y p)

instance IsTurtleNumber n => Subscribed (Turtle n Position) where
    subscribedProxy (Proxy :: Proxy (Turtle n Position)) = subscribedROS $ do
        pose <- subscribeBuffered 1 (turtleString' (Proxy::Proxy n) </> "pose")
        return $ fmap (Turtle . posePosition) pose
        
instance Subscribed (AnyTurtle Position) where
    subscribed = do
        chan <- liftIO $ newChan
        lift $ forM [1..9] $ \i -> do
            t <- subscribeBuffered 1 ("turtle" ++ show i </> "pose")
            flip runHandler_ t $ \v -> lift $ writeChan chan $ AnyTurtle i $ posePosition v
        let chanTopic = Topic $ do
                x <- lift $ readChan chan
                return (return x,chanTopic)
        return $ chanTopic
        
poseOrientation :: Pose -> Orientation
poseOrientation p = Orientation (realToFrac $ Pose._theta p)
        
instance IsTurtleNumber n => Subscribed (Turtle n Orientation) where
    subscribedProxy (Proxy :: Proxy (Turtle n Orientation)) = subscribedROS $ do
        odom <- subscribeBuffered 1 (turtleString' (Proxy::Proxy n) </> "pose")
        return $ fmap (Turtle . poseOrientation) odom

instance Subscribed (AnyTurtle Orientation) where
    subscribed = do
        chan <- liftIO $ newChan
        lift $ forM [1..9] $ \i -> do
            t <- subscribeBuffered 1 ("turtle" ++ show i </> "pose")
            flip runHandler_ t $ \v -> lift $ writeChan chan $ AnyTurtle i $ poseOrientation v
        let chanTopic = Topic $ do
                x <- lift $ readChan chan
                return (return x,chanTopic)
        return $ chanTopic
       
poseVelocity :: Pose -> Velocity
poseVelocity p = Velocity (realToFrac $ _linear_velocity p) (realToFrac $ _angular_velocity p)
        
instance IsTurtleNumber n => Subscribed (Turtle n Velocity) where
    subscribedProxy (Proxy :: Proxy (Turtle n Velocity)) = subscribedROS $ do
        odom <- subscribeBuffered 1 (turtleString' (Proxy::Proxy n) </> "pose")
        return $ fmap (Turtle . poseVelocity) odom
 
instance Subscribed (AnyTurtle Velocity) where
    subscribed = do
        chan <- liftIO $ newChan
        lift $ forM [1..9] $ \i -> do
            t <- subscribeBuffered 1 ("turtle" ++ show i </> "pose")
            flip runHandler_ t $ \v -> lift $ writeChan chan $ AnyTurtle i $ poseVelocity v
        let chanTopic = Topic $ do
                x <- lift $ readChan chan
                return (return x,chanTopic)
        return $ chanTopic

-- ** Turtlesim params

-- | The default position of a turtle
turtlesimDefaultPosition :: Position
turtlesimDefaultPosition = Position (250/45) (250/45)

-- | The default orientation of a turtle
turtlesimDefaultOrientation :: Orientation
turtlesimDefaultOrientation = Orientation 0

data Color = Color { r :: Int, g :: Int, b :: Int }
    deriving (Show, Eq, Ord, Typeable, G.Generic)

instance D.Default Color

black = Color 0 0 0
red = Color 255 0 0
green = Color 0 255 0
blue = Color 0 0 255
white = Color 255 255 255

data Background = Background Color
    deriving (Show, Eq, Ord, Typeable, G.Generic)

instance D.Default Background

instance Subscribed Background where
    subscribed = do
        r <- lift $ getParameter'' "background_r"
        g <- lift $ getParameter'' "background_g"
        b <- lift $ getParameter'' "background_b"
        let readVar mb def = do
                case mb of
                    Nothing -> return def
                    Just mv -> liftM (maybe def id) $ readDynMVar' mv
        return $ Topic.topicRate defaultRate $ Topic.repeatM $ do
            (r' ::Int) <- liftIO $ readVar r 69
            (g' ::Int) <- liftIO $ readVar g 86
            (b' ::Int) <- liftIO $ readVar b 255
            return $ return $ Background $ Color r' g' b'
        
instance Published Background where
    published t = do
        chan <- liftIO $ newTChanIO
        forkUserNodeIO $ do
            let rec = do
                    Background (Color r g b) <- liftIO $ atomically $ readTChan chan
                    lift $ setParameter "background_r" r
                    lift $ setParameter "background_g" g
                    lift $ setParameter "background_b" b
                    rec
            rec
        return $ flip fmap t $ \stm -> stm >>= \mb -> do
            case mb of
                Nothing -> return ()
                Just c -> writeTChan chan c
        

-- ** Turtlesim services

clear :: Task () ()
clear = CoreTask (callService "clear" EmptyRequest (Proxy::Proxy EmptyResponse)) >> return ()

reset :: Task () ()
reset = CoreTask (callService "reset" EmptyRequest (Proxy::Proxy EmptyResponse)) >> return ()

kill :: TurtleNumber -> Task () ()
kill i = CoreTask (callService "kill" (KillRequest $ "turtle"++show i) (Proxy::Proxy KillResponse)) >> return ()

spawn :: Position -> Orientation -> Task () TurtleNumber
spawn (Position x y) (Orientation o) = CoreTask (callService "spawn" (SpawnRequest (realToFrac x) (realToFrac y) (realToFrac o) "") (Proxy::Proxy SpawnResponse)) >>= (return . maybe 0 (invTurtleString . SpawnResponse._name))

data OnOff = On | Off
    deriving (Show, Eq, Ord, Typeable, G.Generic)
    
instance D.Default OnOff
    
isOn :: OnOff -> Bool
isOn On = True
isOn Off = False

isOff :: OnOff -> Bool
isOff On = False
isOff Off = True
    
data Pen = Pen { penColor :: Color, penWidth :: Int, penOnOff :: OnOff }
    deriving (Show, Eq, Ord, Typeable, G.Generic)

instance D.Default Pen

setPen :: TurtleNumber -> Pen -> Task () ()
setPen i (Pen (Color r g b) width on) = CoreTask (callService (turtleString i </> "set_pen") (SetPenRequest (toEnum r) (toEnum g) (toEnum b) (toEnum width) (if isOff on then 1 else 0)) (Proxy::Proxy SetPenResponse)) >> return ()

teleportAbsolute :: TurtleNumber -> Position -> Orientation -> Task () ()
teleportAbsolute i (Position x y) (Orientation o) = CoreTask (callService (turtleString i </> "teleport_absolute") (TeleportAbsoluteRequest (realToFrac x) (realToFrac y) (realToFrac o)) (Proxy::Proxy TeleportAbsoluteResponse)) >> return ()

teleportRelative :: TurtleNumber -> Distance -> Task () ()
teleportRelative i (Velocity lin ang) = CoreTask (callService (turtleString i </> "teleport_relative") (TeleportRelativeRequest (realToFrac lin) (realToFrac ang)) (Proxy::Proxy TeleportRelativeResponse)) >> return ()

