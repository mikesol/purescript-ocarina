module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude
import Effect (Effect)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame, Scene, WAG)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscGraph)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

type Res
  = String

type SceneSig :: forall k. k -> Type
type SceneSig proof
  = Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proof Res

type FrameSig' step proof a
  = Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proof Res step a

type FrameSig step proof
  = FrameSig' step proof { loop :: LoopSig, iteration :: Int }

type WAGSig' step proof a
  = WAG FFIAudio (Effect Unit) proof Res step a

type WAGSig step proof
  = WAGSig' step proof { loop :: LoopSig, iteration :: Int }

type IxWAGSig' stepA stepB proof a
  = IxWAG FFIAudio (Effect Unit) proof Res stepA stepB a

type IxWAGSig stepA stepB proof
  = IxWAGSig' stepA stepB proof { loop :: LoopSig, iteration :: Int }

type StepSig step proof
  = WAGSig step proof ->
    SceneSig proof

newtype LoopSig
  = LoopSig (forall proof. StepSig SinOscGraph proof)
