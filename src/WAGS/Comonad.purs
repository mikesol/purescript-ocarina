module WAGS.Comonad where

import Prelude

import Control.Comonad (class Comonad, extract)

separate :: forall a b w. Comonad w => (a -> w Unit -> b) -> w a -> b
separate f w = f (extract w) (w $> unit)