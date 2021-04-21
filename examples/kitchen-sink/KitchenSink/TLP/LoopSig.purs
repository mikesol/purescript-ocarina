module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude

import Effect (Effect)
import WAGS.Control.Thunkable (Thunkable)
import WAGS.Control.Types (FrameT, SceneT)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscUniverse)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

type Res = String

type SceneSig :: forall k. k -> Type
type SceneSig proof
  = SceneT (SceneI Unit Unit) FFIAudio (Effect Unit) proof Thunkable Res

type StepSig step proof iu
  = FrameT (SceneI Unit Unit) FFIAudio (Effect Unit) proof Thunkable Res iu step LoopSig ->
    SceneSig proof

newtype LoopSig
  = LoopSig (forall proof iu cb. StepSig (SinOscUniverse cb) proof iu)
