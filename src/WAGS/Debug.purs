module WAGS.Debug where

import Prelude

import Prim.TypeError (class Warn, Above, Quote, Text)

-- | A class used to print the input and output types of any indexed monad.
class IxSpy :: forall k1 k2 k3. (k1 -> k2 -> k3 -> Type) -> k1 -> k2 -> k3 -> Constraint
class IxSpy m i o a where
  ixspy :: m i o a -> m i o a

instance ixspyI :: (Warn ((Text "ixspy") ^^ (Quote (m i o a)))) => IxSpy m i o a where
  ixspy = identity

infixr 5 type Above as ^^
