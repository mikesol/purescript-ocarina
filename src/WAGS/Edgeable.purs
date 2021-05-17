module WAGS.Edgeable where

import Prelude
import Data.Tuple (Tuple(..))

class Edgeable a b | a -> b where
  withEdge :: a -> b

instance edgeableTuple :: Edgeable (Tuple a b) (Tuple a b) where
  withEdge = identity
else instance edgeableRest :: Edgeable a (Tuple a {}) where
  withEdge = flip Tuple {}