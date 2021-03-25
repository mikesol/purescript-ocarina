module Stream6 where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.Lens (lens)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Record (union, modify, delete, insert)
import Type.Data.Peano (Nat, Succ, Z)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- This is decent
-- The next challenge is 

data Val (n :: Nat) a = Val a

derive instance functorVal :: Functor (Val n)

data Stream :: Nat -> Nat -> (Type -> Type) -> Type -> Type -> Type
data Stream i o m env acc = Stream Void

class Actualizable a b | a -> b where
  actualize :: Potential a -> Actual b

class Potentiable b a | b -> a where
  potentiate :: Actual b -> Potential a

-- for now, do not make Apply or Applicative
-- as it is not clear what it would even mean to have f (a -> b) -> f a -> f b for potential _or_ actual
-- in this case, what would f (a -> b) even "mean". what is its index?
newtype Potential a = Potential {
  curIdx :: Int -> Int,
  idx :: Maybe (Int -> Int),
  a :: a
}

instance functorPotential :: Functor Potential where
  map f (Potential a) = Potential (modify (Proxy :: _ "a") f a)

instance foldablePotential :: Foldable Potential where
  foldl f b (Potential {a}) = f b a
  foldr f b (Potential {a}) = f a b
  foldMap f (Potential {a}) = f a

instance traversablePotential :: Traversable Potential where
  traverse f (Potential a) = Potential <$> (insert (Proxy :: _ "a") <$> f a.a <*> pure (delete (Proxy :: _ "a") a))
  sequence = traverse identity

newtype Actual a = Actual {
  curIdx :: Int -> Int,
  idx :: Int -> Int,
  whatChanged :: Array WhatChanged,
  a :: a
}
instance functorActual :: Functor Actual where
  map f (Actual a) = Actual (modify (Proxy :: _ "a") f a)

instance foldableActual :: Foldable Actual where
  foldl f b (Actual {a}) = f b a
  foldr f b (Actual {a}) = f a b
  foldMap f (Actual {a}) = f a

instance traversableActual :: Traversable Actual where
  traverse f (Actual a) = Actual <$> (insert (Proxy :: _ "a") <$> f a.a <*> pure (delete (Proxy :: _ "a") a))
  sequence = traverse identity

data WhatChanged = SinOscFreq Number

mkStream :: forall (ix :: Nat) m env acc a x. Actualizable a x => Val ix (Potential a) /\ (acc -> (Actual x) -> m acc) /\ (env -> acc -> Val (Succ ix) (Actual x) -> Stream (Succ ix) Z m env acc) -> Stream ix Z m env acc
mkStream a = Stream (unsafeCoerce o)
  where
  o :: Val ix (Potential a) /\ (Potential a -> Actual x) /\ (acc -> (Actual x) -> m acc) /\ (env -> acc -> Val (Succ ix) (Actual x) -> Stream (Succ ix) Z m env acc)
  o = let (x /\ y) = a in x /\ actualize /\ y

naught :: forall a. a -> Val Z (Potential a)
naught = Val <<< Potential <<< {  curIdx : identity, idx : Nothing, a: _ }

-- m m for canceler
run :: forall m env acc. Applicative m => acc -> (acc -> m Unit) -> Stream Z Z m env acc -> m (m Unit)
run a b c = pure (pure unit)

newtype Stays = Stays Number
newtype Changes = Changes Number
newtype Is = Is Number

changes :: Number -> Changes
changes = Changes

newtype SinOsc a = SinOsc {freq :: a }

data HNil = HNil

data Gain a = Gain { vol ::  Number } a

instance actualizeSinOscStays :: Actualizable (SinOsc Stays) (SinOsc Is)   where
  actualize (Potential { a: SinOsc { freq: Stays a }, idx, curIdx }) = Actual { a: SinOsc { freq: Is a }, idx: fromMaybe curIdx idx, curIdx: maybe (add 1 <<< curIdx) (const curIdx) idx, whatChanged: [] }

instance actualizeSinOscChanges :: Actualizable (SinOsc Changes) (SinOsc Is)   where
  actualize (Potential { a: SinOsc { freq: Changes a }, idx, curIdx }) = Actual { a: SinOsc { freq: Is a }, idx: fromMaybe curIdx idx, curIdx: maybe (add 1 <<< curIdx) (const curIdx) idx, whatChanged: [SinOscFreq a] }

instance potentiateSinOscIs :: Potentiable (SinOsc Is) (SinOsc Stays)   where
  potentiate (Actual { a: SinOsc { freq: Is a }, idx, curIdx })  = (Potential { a: SinOsc { freq: Stays a }, idx: Just idx, curIdx })

type OutputStream = forall c. c /\ c -> Identity c
{-
gainy :: Gain (Tuple SinOsc HNil) -> Stream Identity Int OutputStream
gainy g = mkStream (g /\ (pure) /\ (\_ (Gain {vol} (SinOsc { freq } /\ _) ) -> piece))
-}

piece :: Stream Z Z Identity Int Int
piece = mkStream (naught (SinOsc { freq: changes 340.0})
      /\ (\acc _ -> pure acc)
      /\ (\env acc sosc -> g (map potentiate sosc))) 
  where
  g :: forall (ix :: Nat). Val ix (Potential (SinOsc Stays)) -> Stream ix Z Identity Int Int
  g i = mkStream  (i
      /\ (\acc _ -> pure acc)
      /\ (\env acc sosc -> g (map potentiate sosc)))

-- (Val ix SinOsc /\ (acc -> SinOsc -> m acc) /\ (env -> acc -> Val (Succ ix) SinOsc -> Stream (Succ ix) Z m env acc))

-- use acc as output, but also as internal immutable consumable