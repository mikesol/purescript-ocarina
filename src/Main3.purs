module Main3 where

import Prelude
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

a :: forall a. Identity a
a = Identity (unsafeCoerce unit)

b :: forall a. Identity a
b = Identity (unsafeCoerce unit)

zip :: forall a b c. Identity a -> Identity b -> Identity (Tuple c c)
zip _ _ = Identity (Tuple (unsafeCoerce unit) (unsafeCoerce unit))

q = do
  x <- a
  y <- b
  z <- zip x y
  pure unit
