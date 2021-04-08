module WAGS.Control.Thunkable where

import Prelude

import Data.Tuple (Tuple(..))

data Thunkable a = Here a | Wait (Unit -> Thunkable a)

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

wait :: forall a. a -> Thunkable a
wait = Wait <<< pure <<< pure