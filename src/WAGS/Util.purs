-- | This module contains utility typeclasses for various type-level programs.
module WAGS.Util where

import Data.Typelevel.Bool (True, False)
import Type.Data.Peano (Nat, Succ, Z)

-- | A gate that outputs `l` as `o` if `tf` is `True` and `r` as `o`
-- | if `tf` is `False`.
class Gate :: forall k1. Type -> k1 -> k1 -> k1 -> Constraint
class Gate tf l r o | tf l r -> o

instance gateTrue :: Gate True l r l

instance gateFalse :: Gate False l r r

-- | Type-equality as a true/false assertion. Like
-- | [TypeEquals](https://pursuit.purescript.org/packages/purescript-type-equality/3.0.0/docs/Type.Equality#t:TypeEquals), but allows us to encode the failure case.
class TypeEqualTF (a :: Type) (b :: Type) (c :: Type) | a b -> c

instance typeEqualTFT :: TypeEqualTF a a True
else instance typeEqualTFF :: TypeEqualTF a b False

-- | Assertion that `a` is less than `b`
class LtEq (a :: Nat) (b :: Nat)

instance ltEqZ :: LtEq Z Z

instance ltEqZ' :: LtEq Z (Succ x)

instance ltEqS :: LtEq x y => LtEq (Succ x) (Succ y)
