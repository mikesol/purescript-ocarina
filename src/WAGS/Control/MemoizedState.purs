module WAGS.Control.MemoizedState where

import Prelude

import Control.Monad.State (class MonadState, class MonadTrans, StateT, put, runStateT, state)
import Control.Monad.State as MT
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

-- | MemoizedStateT combines the `StateT` monad and the `Env` comonad. It is similar to `StateT s (m (Env a))`. It is useful when a state combines elements that are known and unknown. The `proof` term allows producers to lock an initial state in place.
newtype MemoizedStateT (proof :: Type) s m a
  = MemoizedStateT (Tuple (First s) (StateT s m a))

type MemoizedState (proof :: Type) s a
  = MemoizedStateT proof s Identity a

instance functorMemoizedStateT :: Monad m => Functor (MemoizedStateT proof s m) where
  map ab (MemoizedStateT (Tuple s a)) = (MemoizedStateT (Tuple s (ab <$> a)))

instance applyMemoizedStateT :: Monad m => Apply (MemoizedStateT proof s m) where
  apply (MemoizedStateT (Tuple fs f)) (MemoizedStateT (Tuple as a)) = (MemoizedStateT (Tuple (fs <> as) (f <*> a)))

instance applicativeMemoizedStateT :: Monad m => Applicative (MemoizedStateT proof s m) where
  pure a = MemoizedStateT (Tuple (First Nothing) (pure a))

instance bindMemoizedStateT :: Monad m => Bind (MemoizedStateT proof s m) where
  bind (MemoizedStateT (Tuple mas ma)) fmb =
    ( MemoizedStateT
        ( Tuple mas do
            a <- ma
            let
              MemoizedStateT (Tuple _ b) = fmb a
            b
        )
    )

instance monadMemoizedStateT :: Monad m => Monad (MemoizedStateT proof s m)

instance monadStateMemoizedStateT :: Monad m => MonadState s (MemoizedStateT proof s m) where
  state = MemoizedStateT <<< Tuple (First Nothing) <<< state

instance monadTransMemoizedStateT :: MonadTrans (MemoizedStateT proof s) where
  lift = MemoizedStateT <<< Tuple (First Nothing) <<< MT.lift

-- | Run a MemoizedStateT'. The `proof` term should be the same one used to create the initial `MemoizedStateT`. `s -> s` is a transformer of the initial state provided when `makeMemoizedStateT` was called. The `s` value is discarded _unless_ no initial state was provided via a `proof` term.
runMemoizedStateT' :: forall proof s m a. Monad m => MemoizedStateT proof s m a -> proof -> (s -> s) -> s -> m (Tuple a s)
runMemoizedStateT' (MemoizedStateT (Tuple maybe st)) _ trans =
  runStateT
    ( case maybe of
        First (Just x) -> do
          put (trans x)
          st
        First Nothing -> st
    )

-- | `runMemoizedStateT'` for the `Identity` monad.
runMemoizedState' :: forall proof s a. MemoizedState proof s a -> proof -> (s -> s) -> s -> Tuple a s
runMemoizedState' m proof trans s = unwrap (runMemoizedStateT' m proof trans s)

-- | `runMemoizedStateT'` without a transformer for the initial state.
runMemoizedStateT :: forall proof s m a. Monad m => MemoizedStateT proof s m a -> proof -> s -> m (Tuple a s)
runMemoizedStateT m proof = runMemoizedStateT' m proof identity

-- | `runMemoizedStateT'` without a transformer for the initial state & for the `Identity` monad.
runMemoizedState :: forall proof s a. MemoizedState proof s a -> proof -> s -> Tuple a s
runMemoizedState m proof s = unwrap (runMemoizedStateT' m proof identity s)

-- | Given `proof`, create a `MemoizedStateT` with an initial state.
makeMemoizedStateT :: forall proof s m a. Monad m => proof -> s -> a -> MemoizedStateT proof s m a
makeMemoizedStateT _ s a = MemoizedStateT (Tuple (First (Just s)) (pure a))

-- | Given `proof`, create a `MemoizedState` with an initial state.
makeMemoizedState :: forall proof s a. proof -> s -> a -> MemoizedState proof s a
makeMemoizedState _ s a = MemoizedStateT (Tuple (First (Just s)) (pure a))
