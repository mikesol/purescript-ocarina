module WAGS.Control.MemoizedState where

import Prelude

import Control.Monad.State (class MonadState, State, put, runState, state)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Tuple (Tuple(..))

newtype MemoizedState (proof :: Type) s a
  = MemoizedState (Tuple (First s) (State s a))

instance functorMemoizedState :: Functor (MemoizedState proof s) where
  map ab (MemoizedState (Tuple s a)) = (MemoizedState (Tuple s (ab <$> a)))

instance applyMemoizedState :: Apply (MemoizedState proof s) where
  apply (MemoizedState (Tuple fs f)) (MemoizedState (Tuple as a)) = (MemoizedState (Tuple (fs <> as) (f <*> a)))

instance applicativeMemoizedState :: Applicative (MemoizedState proof s) where
  pure a = MemoizedState (Tuple (First Nothing) (pure a))

instance bindMemoizedState :: Bind (MemoizedState proof s) where
  bind (MemoizedState (Tuple mas ma)) fmb =
    ( MemoizedState
        ( Tuple mas do
            a <- ma
            let
              MemoizedState (Tuple _ b) = fmb a
            b
        )
    )

instance monadMemoizedState :: Monad (MemoizedState proof s)

instance monadStateMemoizedState :: MonadState s (MemoizedState proof s) where
  state = MemoizedState <<< Tuple (First Nothing) <<< state

runMemoizedState :: forall proof s a. MemoizedState proof s a -> proof -> s -> Tuple a s
runMemoizedState (MemoizedState (Tuple maybe st)) _ =
  runState
    ( case maybe of
        First (Just x) -> do
          put x
          st
        First Nothing -> st
    )

makeMemoizedState :: forall proof s a. proof -> s -> a -> MemoizedState proof s a
makeMemoizedState _ s a = MemoizedState (Tuple (First (Just s)) (pure a))