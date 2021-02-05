{-# LANGUAGE ScopedTypeVariables, TupleSections, UndecidableInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType, PolyKinds, TypeOperators, TypeFamilies, FlexibleContexts, GADTs #-}

module Rosy.Interface.Task.Types where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.Hierarchy
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (ask)

import Data.Typeable
import Data.Default.Generics
import Data.Map (Map(..))
import qualified Data.Map as Map

import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Set hiding (Proxy(..))
import Type.Compare
import qualified Control.Effect as E

import Graphics.Gloss

import Ros.Node
import Rosy.Controller.Core
--import Rosy.Robot.Kobuki.State as Kobuki
--import Rosy.Viewer.Kobuki.State as Kobuki
--import Rosy.Robot.Kobuki.Core as Kobuki
--import Rosy.Viewer.Kobuki.Core as Kobuki
--import Rosy.Robot.Turtlesim.State as Turtle
--import Rosy.Viewer.Turtlesim.State as Turtle
--import Rosy.Robot.Turtlesim.Core as Turtle
--import Rosy.Viewer.Turtlesim.Core as Turtle
--import Rosy.Interface

import Ros.Topic.Util as Topic
import Ros.Topic as Topic

import Unsafe.Coerce
import System.IO.Unsafe
import GHC.Conc

-- | A 'Task' is a monadic 'Effect' that can be composed sequentially, in the sense that a task will only start after the previous task has finished.
data Task (feedback :: [*]) end where
    Task :: (SubscribeDones (CtrDones action),SubscribeFeedbacks (CtrFeedbacks action),Published init,Controller action) => init -> action -> Task (CtrFeedbacks action) (DoneT action)
    RetTask :: a -> Task '[] a
    BindTask :: (UnionEithers fa fb) => Task fa a -> (a -> Task fb b) -> Task (Union fa fb) b
    SubTask :: (SubsetEithers f1 f2) => Task f1 a -> Proxy f2 -> Task f2 a
    CoreTask :: Node a -> Task '[] a
    
instance E.Effect Task where
    type Unit Task = '[]
    type Plus Task f1 f2 = Union f1 f2
    type Inv Task f1 f2 = UnionEithers f1 f2
    
    return = RetTask
    (>>=) = BindTask
    
instance SubsetEithers f1 f2 => E.Subeffect Task f1 f2 where
    sub m = SubTask m Proxy

instance (SubsetEithers '[] f,UnionEithers f f, f ~ Nub (Sort (f :++ f))) => Functor (Task f) where
    fmap f m = m >>= return . f
instance (SubsetEithers '[] f,UnionEithers f f, f ~ Nub (Sort (f :++ f))) => Applicative (Task f) where
    pure = return
    m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))
instance (SubsetEithers '[] f,UnionEithers f f, f ~ Nub (Sort (f :++ f))) => Monad (Task f) where
    return x = SubTask (RetTask x) Proxy
    m1 >>= f = BindTask m1 f

-- | A 'Task' cancelling event.
data Cancel = Cancel
  deriving (Show,Eq,Ord,Typeable)
  
-- | A type-level tag that allows a 'Task' controller to publish feed messages.
data Feedback a = Feedback { unFeedback :: a }
  deriving (Show,Eq,Ord,Typeable)
instance Typeable a => Subscribed (Feedback a) where
    subscribed = subscribedEvent
instance Typeable a => Published (Feedback a) where
    published = publishedEvent

-- * Type-level trickery

-- | The return type of a 'Task'.
type DoneT a = Eithers (CtrDones a)

type CtrDones a = AsSet (CtrDone a)
type CtrFeedbacks a = AsSet (CtrFeedback a)

class SubscribeDones (xs :: [*]) where
    subscribedDones :: Proxy xs -> UserNode (Topic TIO (STM (Eithers xs)))

instance SubscribeDones '[] where
    subscribedDones _ = subscribedProxy (Proxy::Proxy Never)
    
instance (Typeable x,SubscribeDones xs, Eithers (x ': xs) ~ Either x (Eithers xs)) => SubscribeDones (x ': xs) where
    subscribedDones (Proxy::Proxy (x ': xs)) = do
        t <- liftM (fmap (fmap unDone)) $ subscribedProxy (Proxy::Proxy (Done x))
        ts <- subscribedDones (Proxy::Proxy xs)
        let interleave = either (fmap Left) (fmap Right)
        return $ fmap interleave $ t <+> ts

instance {-# OVERLAPS #-} Typeable x => SubscribeDones '[x] where
    subscribedDones (Proxy::Proxy '[x]) = do
        t <- liftM (fmap (fmap unDone)) $ subscribedProxy (Proxy::Proxy (Done x))
        return t
        
class SubscribeFeedbacks (xs :: [*]) where
    subscribedFeedbacks :: Proxy xs -> UserNode (Topic TIO (STM (Eithers xs)))

instance SubscribeFeedbacks '[] where
    subscribedFeedbacks _ = subscribedProxy (Proxy::Proxy Never)
    
instance (Typeable x,SubscribeFeedbacks xs, Eithers (x ': xs) ~ Either x (Eithers xs)) => SubscribeFeedbacks (x ': xs) where
    subscribedFeedbacks (Proxy::Proxy (x ': xs)) = do
        t <- liftM (fmap (fmap unFeedback)) $ subscribedProxy (Proxy::Proxy (Feedback x))
        ts <- subscribedFeedbacks (Proxy::Proxy xs)
        let interleave = either (fmap Left) (fmap Right)
        return $ fmap interleave $ t <+> ts

instance {-# OVERLAPS #-} Typeable x => SubscribeFeedbacks '[x] where
    subscribedFeedbacks (Proxy::Proxy '[x]) = do
        t <- liftM (fmap (fmap unFeedback)) $ subscribedProxy (Proxy::Proxy (Feedback x))
        return t

data Done end = Done { unDone :: end }
  deriving (Show,Eq,Ord,Typeable)
instance Typeable end => Subscribed (Done end) where
    subscribed = subscribedEvent
instance Typeable end => Published (Done end) where
    published = publishedEvent
    
type family CtrDone (a :: *) :: [*] where
    CtrDone [a] = CtrDone a
    CtrDone (a,b) = CtrDone a :++ CtrDone b
    CtrDone (a,b,c) = CtrDone a :++ CtrDone b :++ CtrDone c
    CtrDone (a,b,c,d) = CtrDone a :++ CtrDone b :++ CtrDone c :++ CtrDone d
    CtrDone (a,b,c,d,e) = CtrDone a :++ CtrDone b :++ CtrDone c :++ CtrDone d :++ CtrDone e
    CtrDone (a,b,c,d,e,f) = CtrDone a :++ CtrDone b :++ CtrDone c :++ CtrDone d :++ CtrDone e :++ CtrDone f
    CtrDone (a,b,c,d,e,f,g) = CtrDone a :++ CtrDone b :++ CtrDone c :++ CtrDone d :++ CtrDone e :++ CtrDone f :++ CtrDone g
    CtrDone (a,b,c,d,e,f,g,h) = CtrDone a :++ CtrDone b :++ CtrDone c :++ CtrDone d :++ CtrDone e :++ CtrDone f :++ CtrDone g :++ CtrDone h
    CtrDone (a,b,c,d,e,f,g,h,i) = CtrDone a :++ CtrDone b :++ CtrDone c :++ CtrDone d :++ CtrDone e :++ CtrDone f :++ CtrDone g :++ CtrDone h :++ CtrDone i
    CtrDone (Done end) = '[end]
    CtrDone (Either a b) = CtrDone a :++ CtrDone b
    CtrDone (Maybe a) = CtrDone a
    CtrDone (a -> b) = CtrDone b
    CtrDone a = '[]

type family CtrFeedback (a :: *) :: [*] where
    CtrFeedback [a] = CtrFeedback a
    CtrFeedback (a,b) = CtrFeedback a :++ CtrFeedback b
    CtrFeedback (a,b,c) = CtrFeedback a :++ CtrFeedback b :++ CtrFeedback c
    CtrFeedback (a,b,c,d) = CtrFeedback a :++ CtrFeedback b :++ CtrFeedback c :++ CtrFeedback d
    CtrFeedback (a,b,c,d,e) = CtrFeedback a :++ CtrFeedback b :++ CtrFeedback c :++ CtrFeedback d :++ CtrFeedback e
    CtrFeedback (a,b,c,d,e,f) = CtrFeedback a :++ CtrFeedback b :++ CtrFeedback c :++ CtrFeedback d :++ CtrFeedback e :++ CtrFeedback f
    CtrFeedback (a,b,c,d,e,f,g) = CtrFeedback a :++ CtrFeedback b :++ CtrFeedback c :++ CtrFeedback d :++ CtrFeedback e :++ CtrFeedback f :++ CtrFeedback g
    CtrFeedback (a,b,c,d,e,f,g,h) = CtrFeedback a :++ CtrFeedback b :++ CtrFeedback c :++ CtrFeedback d :++ CtrFeedback e :++ CtrFeedback f :++ CtrFeedback g :++ CtrFeedback h
    CtrFeedback (a,b,c,d,e,f,g,h,i) = CtrFeedback a :++ CtrFeedback b :++ CtrFeedback c :++ CtrFeedback d :++ CtrFeedback e :++ CtrFeedback f :++ CtrFeedback g :++ CtrFeedback h :++ CtrFeedback i
    CtrFeedback (Feedback end) = '[end]
    CtrFeedback (Either a b) = CtrFeedback a :++ CtrFeedback b
    CtrFeedback (Maybe a) = CtrFeedback a
    CtrFeedback (a -> b) = CtrFeedback b
    CtrFeedback a = '[]
    
type instance Cmp (a :: *) (b :: *) = CmpType a b
    
class NilEithers' (xs :: [*]) where
    nilEithers' :: Proxy xs -> Eithers' xs
    isNilEithers' :: Proxy xs -> Eithers' xs -> Bool

instance NilEithers' '[] where
    nilEithers' p = ()
    isNilEithers' p _ = True

instance NilEithers' xs => NilEithers' (x ': xs) where
    nilEithers' (_::Proxy (x ': xs)) = Right $ nilEithers' (Proxy::Proxy xs)
    isNilEithers' _ (Left _) = False
    isNilEithers' (_::Proxy (x ': xs)) (Right xs) = isNilEithers' (Proxy::Proxy xs) xs

class AppendEithers' (a :: [*]) (b :: [*]) where
    leftAppendEithers' :: Proxy a -> Proxy b -> Eithers' a -> Eithers' (a :++ b)
    rightAppendEithers' :: Proxy a -> Proxy b -> Eithers' b -> Eithers' (a :++ b)
    
instance NilEithers' b => AppendEithers' '[] b where
    leftAppendEithers' pa pb a = nilEithers' pb
    rightAppendEithers' pa pb b = b
    
instance (AppendEithers' as b) => AppendEithers' (a ': as) b where
    leftAppendEithers' pa pb (Left a) = Left a
    leftAppendEithers' (pa::Proxy (a ': as)) pb (Right as) = Right $ leftAppendEithers' (Proxy::Proxy as) pb as
    rightAppendEithers' (pa::Proxy (a ': as)) pb b = Right $ rightAppendEithers' (Proxy::Proxy as) pb b
    
class IfEithers' g where
    ifEithers' :: Proxy g -> Proxy l -> Proxy r -> Eithers' l -> Eithers' r -> Eithers' (If g l r)

instance IfEithers' True where
    ifEithers' _ _ _ l r = l

instance IfEithers' False where
    ifEithers' _ _ _ l r = r
    
class FilterEithers' (f :: Flag) (p :: *) (xs :: [*]) where
    filterEithers' :: Proxy f -> Proxy p -> Proxy xs -> Eithers' xs -> Eithers' (Filter f p xs)
    
instance FilterEithers' f p '[] where
    filterEithers' pf pp pxs xs = xs
    
instance (FilterEithers' 'FMin p xs,NilEithers' (Filter 'FMin p xs),IfEithers' (Cmp x p == LT)) => FilterEithers' FMin p (x ': xs) where
    filterEithers' pf pp pxs (Left x) = ifEithers' (Proxy::Proxy (Cmp x p == LT)) (Proxy::Proxy (x ': (Filter FMin p xs))) (Proxy::Proxy (Filter FMin p xs)) (Left x) (nilEithers' (Proxy::Proxy (Filter FMin p xs)))
    filterEithers' pf pp pxs (Right y) = ifEithers' (Proxy::Proxy (Cmp x p == LT)) (Proxy::Proxy (x ': (Filter FMin p xs))) (Proxy::Proxy (Filter FMin p xs)) (Right $ filterEithers' pf pp (Proxy::Proxy xs) y) (filterEithers' pf pp (Proxy::Proxy xs) y)
   
instance (FilterEithers' 'FMax p xs,IfEithers' ((Cmp x p == 'GT) || (Cmp x p == 'EQ)),NilEithers' (Filter 'FMax p xs)) => FilterEithers' FMax p (x ': xs) where
    filterEithers' pf pp pxs (Left x) = ifEithers' (Proxy::Proxy (Cmp x p == GT || Cmp x p == EQ)) (Proxy::Proxy (x ': (Filter FMax p xs))) (Proxy::Proxy (Filter FMax p xs)) (Left x) (nilEithers' (Proxy::Proxy (Filter FMax p xs)))
    filterEithers' pf pp pxs (Right y) = ifEithers' (Proxy::Proxy (Cmp x p == GT || Cmp x p == EQ)) (Proxy::Proxy (x ': (Filter FMax p xs))) (Proxy::Proxy (Filter FMax p xs)) (Right $ filterEithers' pf pp (Proxy::Proxy xs) y) (filterEithers' pf pp (Proxy::Proxy xs) y)
  
class SortEithers' (a :: [*]) where
    sortEithers' :: Proxy a -> Eithers' a -> Eithers' (Sort a)

instance SortEithers' '[] where
    sortEithers' pa a = a

instance (NilEithers' (Filter 'FMin x xs),AppendEithers' (Sort (Filter 'FMin x xs)) '[x],AppendEithers' (Sort (Filter 'FMin x xs) :++ '[x]) (Sort (Filter 'FMax x xs)),SortEithers' (Filter 'FMax x xs),FilterEithers' 'FMax x xs,SortEithers' (Filter 'FMin x xs),FilterEithers' 'FMin x xs) => SortEithers' (x ': xs) where
    sortEithers' (pa::Proxy (x ': xs)) (Left x) = leftAppendEithers' (Proxy::Proxy ((Sort (Filter FMin x xs)) :++ '[x])) (Proxy::Proxy (Sort (Filter FMax x xs))) $ rightAppendEithers' (Proxy::Proxy (Sort (Filter FMin x xs))) (Proxy::Proxy '[x]) $ Left x
    sortEithers' (pa::Proxy (x ': xs)) (Right xs) = if isNilEithers' (Proxy::Proxy (Filter FMin x xs)) minxs
        then rightAppendEithers' (Proxy::Proxy ((Sort (Filter FMin x xs)) :++ '[x])) (Proxy::Proxy (Sort (Filter FMax x xs))) $ sortEithers' (Proxy::Proxy (Filter FMax x xs)) $ filterEithers' (Proxy::Proxy 'FMax) (Proxy::Proxy x) (Proxy::Proxy xs) xs
        else leftAppendEithers' (Proxy::Proxy ((Sort (Filter FMin x xs)) :++ '[x])) (Proxy::Proxy (Sort (Filter FMax x xs))) $ leftAppendEithers' (Proxy::Proxy (Sort (Filter FMin x xs))) (Proxy::Proxy '[x]) $ sortEithers' (Proxy::Proxy (Filter FMin x xs)) minxs
      where
        minxs = filterEithers' (Proxy::Proxy 'FMin) (Proxy::Proxy x) (Proxy::Proxy xs) xs
  
class NubEithers' (a :: [*]) where
    nubEithers' :: Proxy a -> Eithers' a -> Eithers' (Nub a)

instance NubEithers' '[] where
    nubEithers' pa a = a

instance NubEithers' '[e] where
    nubEithers' pa a = a

instance {-# OVERLAPPABLE #-} (Eithers' (Nub (e ': f ': s)) ~ Either e (Eithers' (Nub (f ': s))), NubEithers' (f ': s)) => NubEithers' (e ': f ': s) where
    nubEithers' (p::Proxy (e ': f ': s)) (Left e) = Left e
    nubEithers' (p::Proxy (e ': f ': s)) (Right fs) = Right $ nubEithers' (Proxy::Proxy (f ': s)) fs

instance {-# OVERLAPS #-} (NubEithers' (e ': s)) => NubEithers' (e ': e ': s) where
    nubEithers' (p::Proxy (e ': e ': s)) (Left e) = nubEithers' (Proxy::Proxy (e ': s)) $ Left e
    nubEithers' (p::Proxy (e ': e ': s)) (Right (Left e)) = nubEithers' (Proxy::Proxy (e ': s)) $ Left e
    nubEithers' (p::Proxy (e ': e ': s)) (Right (Right s)) = nubEithers' (Proxy::Proxy (e ': s)) $ Right s

class UnionEithers' (a :: [*]) (b :: [*]) where
    leftUnionEithers'  :: Proxy a -> Proxy b -> Eithers' a -> Eithers' (Union a b)
    rightUnionEithers' :: Proxy a -> Proxy b -> Eithers' b -> Eithers' (Union a b)
    
instance (AppendEithers' a b,SortEithers' (a :++ b),NubEithers' (Sort (a :++ b))) => UnionEithers' a b where
    leftUnionEithers' pa pb a = nubEithers' (Proxy::Proxy (Sort (a :++ b))) $ sortEithers' (Proxy::Proxy (a :++ b)) $ leftAppendEithers' pa pb a
    rightUnionEithers' pa pb b = nubEithers' (Proxy::Proxy (Sort (a :++ b))) $ sortEithers' (Proxy::Proxy (a :++ b)) $ rightAppendEithers' pa pb b

class SubsetEithers' (a :: [*]) (b :: [*]) where
    subsetEithers' :: Proxy a -> Proxy b -> Eithers' a -> Eithers' b

instance SubsetEithers' '[] '[] where
    subsetEithers' _ _ = id

instance {-# OVERLAPPABLE #-} SubsetEithers' s t => SubsetEithers' s (x ': t) where
    subsetEithers' _ _ s = Right $ subsetEithers' (Proxy::Proxy s) (Proxy::Proxy t) s

instance {-# OVERLAPS #-} SubsetEithers' s t => SubsetEithers' (x ': s) (x ': t) where
    subsetEithers' _ _ (Left x) = Left x
    subsetEithers' _ _ (Right s) = Right $ subsetEithers' (Proxy::Proxy s) (Proxy::Proxy t) s

class UnionEithers (a :: [*]) (b :: [*]) where
    leftUnionEithers  :: Proxy a -> Proxy b -> Eithers a -> Maybe (Eithers (Union a b))
    rightUnionEithers :: Proxy a -> Proxy b -> Eithers b -> Maybe (Eithers (Union a b))
    
instance (ToFromEithers a,ToFromEithers b,ToFromEithers (Union a b),UnionEithers' a b) => UnionEithers a b where
    leftUnionEithers pa pb a = toEithers (Proxy::Proxy (Union a b)) $ leftUnionEithers' pa pb (fromEithers pa a)
    rightUnionEithers pa pb b = toEithers (Proxy::Proxy (Union a b)) $ rightUnionEithers' pa pb (fromEithers pb b)
    
class SubsetEithers (a :: [*]) (b :: [*]) where
    subsetEithers  :: Proxy a -> Proxy b -> Eithers a -> Maybe (Eithers b)
    
instance (ToFromEithers a,ToFromEithers b,ToFromEithers (Union a b),SubsetEithers' a b) => SubsetEithers a b where
    subsetEithers pa pb a = toEithers (Proxy::Proxy b) $ subsetEithers' pa pb (fromEithers pa a)
    
type family Eithers (x :: [*]) :: * where
            Eithers '[] = Never
            Eithers '[x] = x
            Eithers (x ': xs) = Either x (Eithers xs)

type family Eithers' (x :: [*]) :: * where
            Eithers' '[] = ()
            Eithers' (x ': xs) = Either x (Eithers' xs)

class ToFromEithers (xs :: [*]) where
    toEithers :: Proxy xs -> Eithers' xs -> Maybe (Eithers xs)
    fromEithers :: Proxy xs -> Eithers xs -> Eithers' xs

instance ToFromEithers '[] where
    toEithers p a = Nothing
    fromEithers p a = ()

instance (Eithers (x ': xs) ~ Either x (Eithers xs),ToFromEithers xs) => ToFromEithers (x ': xs) where
    toEithers p (Left x) = Just $ Left x
    toEithers (p::Proxy (x ': xs)) (Right y) = fmap Right $ toEithers (Proxy::Proxy xs) y
    fromEithers p (Left x) = Left x
    fromEithers (p::Proxy (x ': xs)) (Right y) = Right $ fromEithers (Proxy::Proxy xs) y
   
instance {-# OVERLAPS #-} ToFromEithers '[x] where
    toEithers p (Left x) = Just x
    toEithers p (Right _) = Nothing
    fromEithers p x = Left x 


