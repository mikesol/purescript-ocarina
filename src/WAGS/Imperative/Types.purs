-- | Type definitions for the imperative graph builder.
module WAGS.Imperative.Types where

-- | A poly-kinded pair of types.
data TypePair :: forall k l. k -> l -> Type
data TypePair a b

infixr 6 type TypePair as \/
