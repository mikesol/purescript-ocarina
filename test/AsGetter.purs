module Test.AsGetter where

import Prelude
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Graph.Constructors (Gain(..), PlayBuf(..))
import WAGS.Graph.Getter (asGetter)
import WAGS.Graph.Optionals (gain, playBuf)
import WAGS.Graph.Parameter (param)

testAsGetter :: Spec Unit
testAsGetter =
  describe "test asGetter" do
    it "turns all setters to the identity function" do
      let
        x = gain 1.0 (playBuf { playbackRate: 3.0 } "hello")

        (Gain fg (PlayBuf _ _ _ fr)) = asGetter x
      (unwrap (fg (param 3.0))).param `shouldEqual` 3.0
      (unwrap (fr (param 4.2))).param `shouldEqual` 4.2
