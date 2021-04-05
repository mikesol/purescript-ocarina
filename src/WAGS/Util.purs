module WAGS.Util where

import Data.Typelevel.Bool (True, False)

class Gate :: forall k1. Type -> k1 -> k1 -> k1 -> Constraint
class Gate tf l r o | tf l r -> o

instance gateTrue :: Gate True l r l

instance gateFalse :: Gate False l r r
