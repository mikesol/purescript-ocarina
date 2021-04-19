module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude

import Effect (Effect)
import WAGS.Control.Types (Frame, Scene)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscUniverse)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

newtype LoopSig = LoopSig (forall proof iu cb. Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proof iu (SinOscUniverse cb) LoopSig ->  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proof)