module Test.NE2CF where

import Prelude
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Error.Class (class MonadThrow)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Ord (abs)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import WAGS.NE2CF (makeLoopingPiecewise)

two :: ∀ (t ∷ Type). Semiring t ⇒ t
two = add one one

epsilon :: ∀ (t ∷ Type). EuclideanRing t ⇒ t
epsilon = one `div` (two * two * two * two * two * two * two)

shouldEqualIsh :: ∀ m t. MonadThrow Error m ⇒ Show t ⇒ Eq t ⇒ Ord t ⇒ EuclideanRing t ⇒ t → t → m Unit
shouldEqualIsh a b =
  when (abs (a - b) >= epsilon)
    $ fail
    $ show a
    <> " ≠ (ish) "
    <> show b

testNE2CF :: Spec Unit
testNE2CF =
  describe "Tests piecewise function" do
    describe "Tests looping piecewise function" do
      it "Loops" do
        let
          lpw = makeLoopingPiecewise 2.0 ((0.0 /\ 0.0) :| (1.0 /\ 0.1) : (1.5 /\ 1.0) : Nil)

          v0 = lpw { time: 0.0, headroom: 0.1 }

          v1 = (tail v0) { time: 0.4, headroom: 0.1 }

          v2 = (tail v1) { time: 1.3, headroom: 0.1 }

          v3 = (tail v2) { time: 1.7, headroom: 0.1 }

          v4 = (tail v3) { time: 2.0, headroom: 0.1 }

          v5 = (tail v4) { time: 2.4, headroom: 0.1 }
        head v0 `shouldEqualIsh` pure 0.0
        head v1 `shouldEqualIsh` pure 0.04
        head v2 `shouldEqualIsh` pure 0.64
        head v3 `shouldEqualIsh` pure 0.6
        head v4 `shouldEqualIsh` pure 0.00
        head v5 `shouldEqualIsh` pure 0.04
