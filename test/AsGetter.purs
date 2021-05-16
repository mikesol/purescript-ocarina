module Test.AsGetter where

import Prelude
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Graph.AudioUnit (Gain(..), PlayBuf(..))
import WAGS.Graph.Getter (asGetter)
import WAGS.Graph.Optionals (gain_, playBuf_)
import WAGS.Graph.Parameter (param)

testAsGetter :: Spec Unit
testAsGetter =
  describe "test asGetter" do
    it "turns all setters to the identity function" do
      let
        x = gain_ 1.0

        Gain x' = asGetter x

        y = playBuf_ { playbackRate: 3.0 } "hello"

        PlayBuf _ _ _ y' = asGetter y
      (unwrap (x' (param 3.0))).param `shouldEqual` 3.0
      (unwrap (y' (param 4.2))).param `shouldEqual` 4.2
