module WAGS.Control.Types
  ( FrameT(..)
  , Frame
  , AudioState
  , AudioState'
  , InitialUniverse
  , InitialFrameT
  , InitialFrame
  , Frame0
  , SceneT(..)
  , SceneT'
  , Scene
  , oneFrame
  , oneFrame'
  , oneFrameT
  , oneFrameT'
  ) where

import Prelude

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.Identity (Identity)
import Data.Map as M
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Peano (Z)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Control.MemoizedState (MemoizedStateT)
import WAGS.Rendered (AnAudioUnit, Instruction)
import WAGS.Universe.Bin (D0)
import WAGS.Universe.Graph (InitialGraph)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (Universe, UniverseC)

type AudioState' env
  = { env :: env
    , currentIdx :: Int
    , instructions :: Array Instruction
    , internalNodes :: M.Map Int (AnAudioUnit)
    , internalEdges :: M.Map Int (Set Int)
    }

type AudioState env proof m a
  = (MemoizedStateT proof (AudioState' env) m) a

newtype FrameT (env :: Type) (proof :: Type) (m :: Type -> Type) (iu :: Universe) (ou :: Universe) (a :: Type)
  = FrameT (AudioState env proof m a)

type Frame (env :: Type) (proof :: Type) (iu :: Universe) (ou :: Universe) (a :: Type)
  = FrameT env proof Identity iu ou a

data Frame0

type InitialUniverse
  = UniverseC D0 InitialGraph Z SkolemListNil

type InitialFrameT env m a
  = FrameT env Frame0 m InitialUniverse InitialUniverse a

type InitialFrame env a
  = Frame env Frame0 InitialUniverse InitialUniverse a

instance frameFunctor :: Monad m => Functor (FrameT env proof m i o) where
  map f (FrameT (a)) = FrameT (f <$> a)

instance frameIxFunctor :: Monad m => IxFunctor (FrameT env proof m) where
  imap f (FrameT (a)) = FrameT (f <$> a)

instance frameIxApplicative :: Monad m => IxApply (FrameT env proof m) where
  iapply (FrameT (f)) (FrameT (a)) = FrameT ((f <*> a))

instance frameIxApply :: Monad m => IxApplicative (FrameT env proof m) where
  ipure a = FrameT $ (pure a)

instance frameIxBind :: Monad m => IxBind (FrameT env proof m) where
  ibind (FrameT (monad)) function = FrameT ((monad >>= ((\(FrameT x) -> x) <<< function)))

instance frameIxMonad :: Monad m => IxMonad (FrameT env proof m)

data SceneT :: forall k. Type -> k -> (Type -> Type) -> Type
data SceneT env proof m
  = SceneT (env -> m (SceneT' env proof m))

type Scene :: forall k. Type -> k -> Type
type Scene env proof
  = SceneT env proof Identity

type SceneT' :: forall k. Type -> k -> (Type -> Type) -> Type
type SceneT' env proof m
  = { nodes :: M.Map Int AnAudioUnit
    , edges :: M.Map Int (Set Int)
    , instructions :: Array Instruction
    , next :: SceneT env proof m
    }

type Scene' :: forall k. Type -> k -> Type
type Scene' env proof
  = SceneT' env proof Identity

oneFrameT :: forall env proofA m. Monad m => SceneT env proofA m -> env -> (forall proofB. m (SceneT' env proofB m))
oneFrameT (SceneT f) = (unsafeCoerce :: (env -> m (SceneT' env proofA m)) -> (env -> (forall proofB. m (SceneT' env proofB m))) ) f
  

oneFrame :: forall env proofA. Scene env proofA -> env -> (forall proofB. Scene' env proofB)
oneFrame m s = unwrap (oneFrameT m s)

oneFrameT' :: forall env proofA m. Monad m => SceneT env proofA m -> env -> (forall proofB. m (M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array Instruction /\ SceneT env proofB m))
oneFrameT' s e = go <$> (oneFrameT s e)
  where
  go x = nodes /\ edges /\ instructions /\ next
    where
    { nodes, edges, instructions, next } = x

oneFrame' :: forall env proofA. Scene env proofA -> env -> (forall proofB. (M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array Instruction /\ Scene env proofB))
oneFrame' s e = unwrap (oneFrameT' s e)
