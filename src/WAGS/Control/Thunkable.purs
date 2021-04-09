module WAGS.Control.Thunkable where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Data.Identity (Identity)
import Data.Traversable (class Foldable, class Traversable, foldMapDefaultR, sequence, traverseDefault)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import WAGS.Control.MemoizedState (MemoizedStateT)

data Thunkable a
  = Here a
  | Wait (Unit -> Thunkable a)

isWait :: forall a. Thunkable a -> Boolean
isWait = case _ of
  Wait x -> true
  Here x -> false

isHere :: forall a. Thunkable a -> Boolean
isHere = case _ of
  Wait x -> false
  Here x -> true

runThunkable :: forall a. Thunkable a -> a
runThunkable (Here a) = a

runThunkable (Wait f) = runThunkable (f unit)

runThunkableWithCount :: forall a. Thunkable a -> Tuple Int a
runThunkableWithCount (Here a) = Tuple 0 a

runThunkableWithCount (Wait f) = Tuple (x + 1) y
  where
  Tuple x y = runThunkableWithCount (f unit)

thunkThunkable :: forall a. Thunkable a -> Thunkable a
thunkThunkable (Here a) = Here a

thunkThunkable (Wait f) = f unit

monadifyThunkable :: forall m. Monad m => Thunkable ~> m
monadifyThunkable = intercalateThunkable (pure unit)

intercalateThunkable :: forall m. Monad m => m Unit -> Thunkable ~> m
intercalateThunkable m t = case (thunkThunkable t) of
  Here a -> pure a
  Wait f -> m >>= \_ -> intercalateThunkable m (f unit)

instance semigroupThunkable :: Semigroup a => Semigroup (Thunkable a) where
  append (Here a) (Here b) = Here (a <> b)
  append (Here f) (Wait fa) = Wait (const $ append (pure f) (fa unit))
  append (Wait ff) (Here a) = Wait (const $ append (ff unit) (pure a))
  append (Wait ff) (Wait fa) = Wait (const $ append (ff unit) (Wait fa))

instance foldableThunkable :: Foldable Thunkable where
  foldl bab b = bab b <<< runThunkable
  foldr abb b a = abb (runThunkable a) b
  foldMap = foldMapDefaultR

instance traversableThunkable :: Traversable Thunkable where
  traverse = traverseDefault
  sequence (Here ma) = map Here ma
  sequence (Wait fma) = sequence (fma unit)

instance monoidThunkable :: Monoid a => Monoid (Thunkable a) where
  mempty = Here mempty

instance semiringThunkable :: Semiring a => Semiring (Thunkable a) where
  zero = Here zero
  one = Here one
  add (Here a) (Here b) = Here (add a b)
  add (Here f) (Wait fa) = Wait (const $ add (pure f) (fa unit))
  add (Wait ff) (Here a) = Wait (const $ add (ff unit) (pure a))
  add (Wait ff) (Wait fa) = Wait (const $ add (ff unit) (Wait fa))
  mul (Here a) (Here b) = Here (mul a b)
  mul (Here f) (Wait fa) = Wait (const $ mul (pure f) (fa unit))
  mul (Wait ff) (Here a) = Wait (const $ mul (ff unit) (pure a))
  mul (Wait ff) (Wait fa) = Wait (const $ mul (ff unit) (Wait fa))

instance ringThunkable :: Ring a => Ring (Thunkable a) where
  sub (Here a) (Here b) = Here (sub a b)
  sub (Here f) (Wait fa) = Wait (const $ sub (pure f) (fa unit))
  sub (Wait ff) (Here a) = Wait (const $ sub (ff unit) (pure a))
  sub (Wait ff) (Wait fa) = Wait (const $ sub (ff unit) (Wait fa))

instance functorThunkable :: Functor Thunkable where
  map f = case _ of
    Here a -> Here (f a)
    Wait fa -> Wait ((map <<< map) f fa)

instance applyThunkable :: Apply Thunkable where
  apply (Here f) (Here a) = Here (f a)
  apply (Here f) (Wait fa) = Wait (const $ apply (pure f) (fa unit))
  apply (Wait ff) (Here a) = Wait (const $ apply (ff unit) (pure a))
  apply (Wait ff) (Wait fa) = Wait (const $ apply (ff unit) (Wait fa))

instance applicativeThunkable :: Applicative Thunkable where
  pure a = Here a

instance bindThunkable :: Bind Thunkable where
  bind (Here a) fmb = fmb a
  bind (Wait fa) fmb = Wait (const $ bind (fa unit) fmb)

instance monadThunkable :: Monad Thunkable

instance altThunkable :: Alt Thunkable where
  alt (Here a) (Here _) = Here a
  alt (Here a) (Wait _) = Here a
  alt (Wait _) (Here a) = Here a
  alt (Wait a) (Wait b) = Wait (\_ -> alt (a unit) (b unit))

instance plusThunkable :: Plus Thunkable where
  empty = Wait (\_ -> empty)

instance alternativeThunkable :: Alternative Thunkable

class Waitable f where
  wait :: forall a. a -> f a

instance waitableThunkable :: Waitable Thunkable where
  wait = Wait <<< pure <<< pure

instance waitableIdentity :: Waitable Identity where
  wait = pure

instance waitableAff :: Waitable Aff where
  wait = pure

instance waitableMemoizedStateT :: Waitable (MemoizedStateT proof s Thunkable) where
  wait = pure