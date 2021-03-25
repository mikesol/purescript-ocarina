module Stream5 where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Peano (Nat, Succ, Z)
import Unsafe.Coerce (unsafeCoerce)

-- This is decent
-- The next challenge is 

data Val (n :: Nat) a = Val a

data Stream :: Nat -> Nat -> (Type -> Type) -> Type -> Type -> Type
data Stream i o m env acc = Stream Void

class Explicate a b | a -> b, b -> a where
   actualize :: a -> b
   induce :: b -> a

mkStream :: forall (ix :: Nat) m env acc a x. Explicate a x => Val ix a /\ (acc -> x -> m acc) /\ (env -> acc -> Val (Succ ix) x -> Stream (Succ ix) Z m env acc) -> Stream ix Z m env acc
mkStream a = Stream (unsafeCoerce a)

naught :: forall a. a -> Val Z a
naught = Val

-- change eventually to something useful
unVal :: forall i a. Val i a -> a
unVal (Val a) = a

-- m m for canceler
run :: forall m env acc. Applicative m => acc -> (acc -> m Unit) -> Stream Z Z m env acc -> m (m Unit)
run a b c = pure (pure unit)

data SinOsc = SinOsc {freq :: Number}

data HNil = HNil

data Gain a = Gain { vol ::  Number } a

instance explicateSinOsc :: Explicate SinOsc SinOsc where
  actualize = identity
  induce = identity

type OutputStream = forall c. c /\ c -> Identity c
{-
gainy :: Gain (Tuple SinOsc HNil) -> Stream Identity Int OutputStream
gainy g = mkStream (g /\ (pure) /\ (\_ (Gain {vol} (SinOsc { freq } /\ _) ) -> piece))
-}
piece :: Stream Z Z Identity Int Int
piece = f (naught (SinOsc { freq: 340.0}))
   where
   f :: forall (ix :: Nat). Val ix SinOsc -> Stream ix Z Identity Int Int
   f i = mkStream  (i
      /\ (\acc _ -> pure acc)
      /\ (\env acc sosc -> f sosc))

-- (Val ix SinOsc /\ (acc -> SinOsc -> m acc) /\ (env -> acc -> Val (Succ ix) SinOsc -> Stream (Succ ix) Z m env acc))

-- use acc as output, but also as internal immutable consumable