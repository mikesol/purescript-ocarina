-- | This module contains utility typeclasses for various type-level programs.
module WAGS.Util where

import Prelude hiding (Ordering(..))

import Data.Typelevel.Bool (True, False)
import Math (pow)
import Prim.Ordering (Ordering, LT, GT, EQ)
import Prim.RowList (RowList)
import Prim.RowList as RL
import Prim.Symbol as Sym
import Type.Data.Peano (Nat, Succ, Z)

-- | A gate that outputs `l` as `o` if `tf` is `True` and `r` as `o`
  -- | if `tf` is `False`.
class Gate :: forall k1. Type -> k1 -> k1 -> k1 -> Constraint
class Gate tf l r o | tf l r -> o

instance gateTrue :: Gate True l r l

instance gateFalse :: Gate False l r r

-- | A gate that outputs `l` as `o` if `ord` is `EQ` and `r` as `o`
  -- | if `ord` is `LT` or `GT`.
class OGate :: forall k1. Ordering -> k1 -> k1 -> k1 -> Constraint
class OGate tf l r o | tf l r -> o

instance oGateEq :: OGate EQ l r l

instance oGateLt :: OGate LT l r r

instance oGateGt :: OGate GT l r r

class CmpEq (c :: Ordering) (tf :: Type) | c -> tf

instance cmpEqLt :: CmpEq LT False

instance cmpEqGt :: CmpEq GT False

instance cmpEqEq :: CmpEq EQ True

class SymEq (s0 :: Symbol) (s1 :: Symbol) (tf :: Type) | s0 s1 -> tf

instance symEq :: (Sym.Compare s0 s1 cmp, CmpEq cmp tf) => SymEq s0 s1 tf

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

-- | Assertion that `sym` is not present in `nodeList`.
class SymInRowList' :: ∀ (k ∷ Type). Type -> Symbol -> RowList k -> Type -> Constraint
class SymInRowList' tf sym nodeList o | tf sym nodeList -> o

instance symInRowList'True :: SymInRowList' True sym rl True

instance symInRowList'False :: SymInRowList' False sym RL.Nil False

instance symInRowListCons ::
  ( SymEq sym h tf
  , SymInRowList' tf sym tail o
  ) =>
  SymInRowList' False sym (RL.Cons h head tail) o

-- | Assertion that `sym` is not present in `nodeList`.
class SymInRowList :: ∀ (k ∷ Type). Symbol -> RowList k -> Type -> Constraint
class SymInRowList sym nodeList tf | sym nodeList -> tf

instance symInRowList :: SymInRowList' False sym rl tf => SymInRowList sym rl tf

class RowListEmpty :: ∀ (k ∷ Type). RowList k -> Type -> Constraint
class RowListEmpty rowList tf | rowList -> tf

instance rowListEmptyNil :: RowListEmpty RL.Nil True

instance rowListEmptyCons :: RowListEmpty (RL.Cons a b c) False

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

calcSlopeExp :: Number -> Number -> Number -> Number -> Number -> Number -> Number
calcSlopeExp x0 y0 x1 y1 exp x' =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      dx = x1 - x0

      x = ((((x' - x0) / dx) `pow` exp) * dx) + x0

      m = (y1 - y0) / dx

      b = y0 - m * x0
    in
      m * x + b