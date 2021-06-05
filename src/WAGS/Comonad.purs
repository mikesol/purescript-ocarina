module WAGS.Comonad where

import Prelude
import Control.Comonad (class Comonad, extract)

separate :: forall a b w. Comonad w => (a -> w Unit -> b) -> w a -> b
separate f w = f (extract w) (w $> unit)

combine :: forall a b u w. Comonad w => (w a -> b) -> a -> w u -> b
combine f a w = f (w $> a)
