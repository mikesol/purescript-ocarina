module FRP.Event.Pure
  ( PureEvent
  , PureEventIO
  , create
  , makePureEvent
  , subscribe
  , module Class
  ) where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Monad.ST (ST, run)
import Control.Monad.ST.Ref as Ref
import Data.Array (deleteBy)
import Data.Compactable (class Compactable)
import Data.Either (Either(..), either, hush)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (sequence_, traverse_)
import Data.Maybe (Maybe(..))
import FRP.Event.Class (class Filterable, class IsEvent, count, filterMap, fix, fold, folded, gate, gateBy, keepLatest, mapAccum, sampleOn, sampleOn_, withLast) as Class
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

-- | An `PureEvent` represents a collection of discrete occurrences with associated
-- | times. Conceptually, an `PureEvent` is a (possibly-infinite) list of values-and-times:
-- |
-- | ```purescript
-- | type PureEvent a = List { value :: a, time :: Time }
-- | ```
-- |
-- | PureEvents are created from real events like timers or mouse clicks, and then
-- | combined using the various functions and instances provided in this module.
-- |
-- | PureEvents are consumed by providing a callback using the `subscribe` function.
newtype PureEvent r a = PureEvent ((a -> ST r Unit) -> ST r (ST r Unit))

instance functorPureEvent :: Functor (PureEvent r) where
  map f (PureEvent e) = PureEvent \k -> e (k <<< f)

instance compactablePureEvent :: Compactable (PureEvent r) where
  compact = filter identity
  separate xs =
    { left:
        filter
          ( case _ of
              Left x -> Just x
              Right _ -> Nothing
          )
          xs
    , right:
        filter
          ( case _ of
              Right x -> Just x
              Left _ -> Nothing
          )
          xs
    }

filter' :: forall r a. (a → Boolean) → PureEvent r a → PureEvent r a
filter' f =
  filter
    ( \a -> case f a of
        true -> Just a
        false -> Nothing
    )

instance filterablePureEvent :: Filterable (PureEvent r) where
  filter = filter'
  filterMap = filter
  partition p xs = { yes: filter' p xs, no: filter' (not <<< p) xs }
  partitionMap f xs =
    { left: filterMap (either Just (const Nothing) <<< f) xs
    , right: filterMap (hush <<< f) xs
    }

instance applyPureEvent :: Apply (PureEvent r) where
  apply (PureEvent e1) (PureEvent e2) =
    PureEvent \k -> do
      latestA <- Ref.new Nothing
      latestB <- Ref.new Nothing
      c1 <-
        e1 \a -> do
          void $ Ref.write (Just a) latestA
          Ref.read latestB >>= traverse_ (k <<< a)
      c2 <-
        e2 \b -> do
          void $ Ref.write (Just b) latestB
          Ref.read latestA >>= traverse_ (k <<< (_ $ b))
      pure (c1 *> c2)

instance applicativePureEvent :: Applicative (PureEvent r) where
  pure a =
    PureEvent \k -> do
      k a
      pure (pure unit)

instance altPureEvent :: Alt (PureEvent r) where
  alt (PureEvent f) (PureEvent g) =
    PureEvent \k -> do
      c1 <- f k
      c2 <- g k
      pure (c1 *> c2)

instance plusPureEvent :: Plus (PureEvent r) where
  empty = PureEvent \_ -> pure (pure unit)

instance alternativePureEvent :: Alternative (PureEvent r)

instance semigroupPureEvent :: Semigroup a => Semigroup (PureEvent r a) where
  append = lift2 append

instance monoidPureEvent :: Monoid a => Monoid (PureEvent r a) where
  mempty = pure mempty

instance eventIsEvent :: Class.IsEvent (PureEvent r) where
  fold = fold
  keepLatest = keepLatest
  sampleOn = sampleOn
  fix = fix

-- | Fold over values received from some `PureEvent`, creating a new `PureEvent`.
fold :: forall r a b. (a -> b -> b) -> PureEvent r a -> b -> PureEvent r b
fold f (PureEvent e) b =
  PureEvent \k -> do
    result <- Ref.new b
    e \a -> Ref.modify (f a) result >>= k

-- | Create an `PureEvent` which only fires when a predicate holds.
filter :: forall r a b. (a -> Maybe b) -> PureEvent r a -> PureEvent r b
filter p (PureEvent e) =
  PureEvent \k ->
    e \a -> case p a of
      Just y -> k y
      Nothing -> pure unit

-- | Create an `PureEvent` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOn :: forall r a b. PureEvent r a -> PureEvent r (a -> b) -> PureEvent r b
sampleOn (PureEvent e1) (PureEvent e2) =
  PureEvent \k -> do
    latest <- Ref.new Nothing
    c1 <-
      e1 \a -> do
        void $ Ref.write (Just a) latest
    c2 <-
      e2 \f -> do
        Ref.read latest >>= traverse_ (k <<< f)
    pure (c1 *> c2)

-- | Flatten a nested `PureEvent`, reporting values only from the most recent
-- | inner `PureEvent`.
keepLatest :: forall r a. PureEvent r (PureEvent r a) -> PureEvent r a
keepLatest (PureEvent e) =
  PureEvent \k -> do
    cancelInner <- Ref.new Nothing
    cancelOuter <-
      e \inner -> do
        Ref.read cancelInner >>= sequence_
        c <- subscribe inner k
        void $ Ref.write (Just c) cancelInner
    pure do
      Ref.read cancelInner >>= sequence_
      cancelOuter

unsafeRecastST :: forall r0 r1. ST r0 ~> ST r1
unsafeRecastST = unsafeCoerce

-- | Compute a fixed point
fix
  :: forall r i o
   . (PureEvent r i -> { input :: PureEvent r i, output :: PureEvent r o })
  -> PureEvent r o
fix f =
  PureEvent \k -> do
    c1 <- subscribe input push
    c2 <- subscribe output k
    pure (c1 *> c2)
  where
  { event, push } = run (unsafeRecastST create)

  { input, output } = f event

-- | Subscribe to an `PureEvent` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe
  :: forall r a
   . PureEvent r a
  -> (a -> ST r Unit)
  -> ST r (ST r Unit)
subscribe (PureEvent e) k = e k

-- | Make an `PureEvent` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makePureEvent
  :: forall r a
   . ((a -> ST r Unit) -> ST r (ST r Unit))
  -> PureEvent r a
makePureEvent = PureEvent

type PureEventIO r a =
  { event :: PureEvent r a
  , push :: a -> ST r Unit
  }

-- | Create an event and a function which supplies a value to that event.
create
  :: forall r a
   . ST r (PureEventIO r a)
create = do
  subscribers <- Ref.new []
  pure
    { event:
        PureEvent \k -> do
          _ <- Ref.modify (_ <> [ k ]) subscribers
          pure do
            _ <- Ref.modify (deleteBy unsafeRefEq k) subscribers
            pure unit
    , push:
        \a -> do
          Ref.read subscribers >>= traverse_ \k -> k a
    }
