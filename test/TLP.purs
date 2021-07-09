module Test.TLP where

import Prelude

import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D0, D1)
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (TSinOsc)
import WAGS.Util (class AutoIncrementingInsert)

testAII0 :: Proxy (D0 /\ ((TSinOsc /\ D0) /\ Unit))
testAII0 = Proxy :: forall val omap. AutoIncrementingInsert TSinOsc Unit val omap => Proxy (val /\ omap)

testAII1 :: Proxy (D1 /\ ((TSinOsc /\ D1) /\ Unit))
testAII1 = Proxy :: forall val omap. AutoIncrementingInsert TSinOsc ((TSinOsc /\ D0) /\ Unit) val omap => Proxy (val /\ omap)