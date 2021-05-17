module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude
import Effect (Effect)
import WAGS.Control.Thunkable (Thunkable)
import WAGS.Control.Types (FrameT, SceneT)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscGraph)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

type Res
  = String

type SceneSig :: forall k. k -> Type
type SceneSig proof
  = SceneT (SceneI Unit Unit) FFIAudio (Effect Unit) proof Thunkable Res

type FrameSig' step proof iu a
  = FrameT (SceneI Unit Unit) FFIAudio (Effect Unit) proof Thunkable Res iu step a

type FrameSig step proof iu
  = FrameSig' step proof iu LoopSig

type StepSig step proof iu
  = FrameSig step proof iu ->
    SceneSig proof

newtype LoopSig
  = LoopSig (forall proof iu. StepSig SinOscGraph proof { | iu })
