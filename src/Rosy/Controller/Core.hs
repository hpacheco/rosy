{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Rosy.Controller.Core where

import Data.UUID.V4 (nextRandom)
import Data.Maybe

import Ros.Node
import Ros.Topic as Topic
import Ros.Topic.Util as Topic

import Control.Monad

type Say = IO ()

say :: String -> Say
say = putStrLn

class Subscribed a where
    subscribed :: Node (Topic IO a)

instance (Subscribed a,Subscribed b) => Subscribed (a,b) where
    subscribed = do
        a <- subscribed
        b <- subscribed
        return $ a `everyNew` b

instance (Subscribed a,Subscribed b,Subscribed c) => Subscribed (a,b,c) where
    subscribed = liftM (fmap (\(a,(b,c)) -> (a,b,c))) subscribed

instance (Subscribed a,Subscribed b,Subscribed c,Subscribed d) => Subscribed (a,b,c,d) where
    subscribed = liftM (fmap (\(a,(b,(c,d))) -> (a,b,c,d))) subscribed

instance (Subscribed a,Subscribed b,Subscribed c,Subscribed d,Subscribed e) => Subscribed (a,b,c,d,e) where
    subscribed = liftM (fmap (\(a,(b,(c,(d,e)))) -> (a,b,c,d,e))) subscribed

instance (Subscribed a,Subscribed b) => Subscribed (Either a b) where
    subscribed = do
        a <- subscribed
        b <- subscribed
        return $ a <+> b

instance (Subscribed a) => Subscribed (Maybe a) where
    subscribed = fmap (fmap (either Just (\() -> Nothing))) subscribed
        
instance Subscribed () where
    subscribed = return $ topicRate 1 $ Topic.repeat ()
    
class Published a where
    published :: Topic IO a -> Node ()
    
instance Published Say where
    published t = runHandler id t >> return ()

(><) :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
(f >< g) (a,b) = (f a,g b)

instance (Published a,Published b) => Published (a,b) where
    published t = do
        (ta,tb) <- liftIO $ fmap (Topic.fst >< Topic.snd) $ tee t
        published ta
        published tb

instance (Published a,Published b,Published c) => Published (a,b,c) where
    published t = published $ fmap (\(a,b,c) -> (a,(b,c))) t
    
instance (Published a,Published b,Published c,Published d) => Published (a,b,c,d) where
    published t = published $ fmap (\(a,b,c,d) -> (a,(b,(c,d)))) t
    
instance (Published a,Published b,Published c,Published d,Published e) => Published (a,b,c,d,e) where
    published t = published $ fmap (\(a,b,c,d,e) -> (a,(b,(c,(d,e))))) t

instance (Published a,Published b) => Published (Either a b) where
    published t = do
        (ta,tb) <- liftIO $ fmap (Topic.left >< Topic.right) $ tee t
        published ta
        published tb

instance (Published a) => Published (Maybe a) where
    published t = do
        let ta = fmap fromJust $ Topic.filter isJust t
        published ta

instance Published () where
    published t = runHandler return t >> return ()

buildController :: (Subscribed a,Published b) => (a -> b) -> Node ()
buildController f = subscribed >>= published . fmap f

-- | Runs a new node that subscribes to inputs of type 'a' and publishes outputs of type 'b'.
runController :: (Subscribed a,Published b) => (a -> b) -> IO ()
runController f = do
    nodename <- nextRandom -- generate a random node name
    --putStrLn $ "Initializing new node with name " ++ show nodename
    runNode (show nodename) (buildController f)

