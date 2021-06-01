module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude

import Data.Identity (Identity)
import Effect (Effect)
import WAGS.Control.Types (FrameT, SceneT)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscGraph)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

type Res
  = String

type SceneSig :: forall k. k -> Type
type SceneSig proof
  = SceneT (SceneI Unit Unit) FFIAudio (Effect Unit) proof Identity Res

type FrameSig' step proof a
  = FrameT (SceneI Unit Unit) FFIAudio (Effect Unit) proof Identity Res step a

type FrameSig step proof
  = FrameSig' step proof { loop :: LoopSig, iteration :: Int }

type StepSig step proof
  = FrameSig step proof ->
    SceneSig proof

newtype LoopSig
  = LoopSig (forall proof. StepSig SinOscGraph proof)
