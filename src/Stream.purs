module Stream(Stream) where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Unsafe.Coerce (unsafeCoerce)

-- In order to remain polymorphic, `a` will need to store
-- an audio unit (ie SinOsc) _and_ its interpreter.
-- The output of the interpreter is a list of commands (on, off, set, etc).

data Stream :: Type -> Type -> Type
data Stream i a = Stream a Void

mkStream :: forall i a. Tuple a (i -> a -> forall b. Stream i b) -> Stream i a
mkStream (a /\ b) = Stream a (unsafeCoerce b)

head :: forall i a. Stream i a -> a
head (Stream a _) = a

tail :: forall i a. Stream i a -> (i -> a -> forall b. Stream i b)
tail (Stream _ b) = unsafeCoerce b
{-

mkStream :: forall i a. Tuple a (i -> a -> forall b. Stream i b) -> Stream i a
tail :: Stream i a -> (i -> a -> forall b. Stream i b)
head :: forall a. Stream i a -> a
-}

data SinOsc = SinOsc {freq :: Number}

data TwoOrOne a b = Two a b | One a

data Fin = Fin

data Gain a = Gain { vol ::  Number } a

data Piece i a =
    Beginning SinOsc (i -> a)
  | Middle (Gain (TwoOrOne SinOsc Void)) (i -> a)
  | End SinOsc (i -> a)

newtype Actual a = Actual {
  idx ::  Int,
  ch ::  Int,
  graph :: Map (Actual a) (Array (Actual a)),
  a :: a
}

mkCofree2 :: ∀ f a. a -> (a -> f (Cofree f a)) -> Cofree f a
mkCofree2 a f = mkCofree a (f a)


piece2 i = mkCofree2 (SinOsc {
  freq : 440.0
  }) (flip Beginning piece2)

{-
class Explicit a b | a -> b where
  explicate :: a -> b

--class MakeIndexed a where
--  makeIndexed :: Indexor -> a -> 

instance explicitSinOsc :: Explicit (Actual SinOsc) (Actual SinOsc) where
  explicate = identity

data Indexor = Indexor Void

mkCofree3 :: ∀ f a b. Explicit a b => Indexor -> a -> (b -> f (Cofree f a)) -> Cofree f a
mkCofree3 ix a f = mkCofree a (f $ explicate a)

myIndexor = Indexor $ unsafeCoerce 1

piece3 i = mkCofree3 myIndexor (SinOsc {
  freq : 440.0
  }) (flip Beginning piece3)-}