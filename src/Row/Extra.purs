module Row.Extra where

import Prim.Row as Row
import Prim.Boolean (True, False)

-- | _The abyss gazes back into thee..._
foreign import data Abyss :: forall k. k

-- | A type class similar to `TypeEquals`, except with a `Boolean`.
class IsEqual :: forall k. k -> k -> Boolean -> Constraint
class IsEqual a b r | a b -> r

instance isEqualEqual :: IsEqual a a True
else instance isEqualNotEqual :: IsEqual a b False

-- | A type class similar to `Prim.Row.Lacks`, except with a
-- | `Boolean`.
class Lacks :: forall k. Symbol -> Row k -> Boolean -> Constraint
class Lacks i r b | i r -> b

instance lacksFieldDefault ::
  ( Row.Cons i Abyss () abyssful
  , Row.Union r abyssful r'
  , Row.Cons i abyss abyssless r'
  , IsEqual abyss Abyss isAbsent
  ) =>
  Lacks i r isAbsent
