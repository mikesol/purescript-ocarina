module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude
import Effect (Effect)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame, Scene, WAG)
import WAGS.Example.KitchenSink.Types.StartGraph (StartGraph)
import WAGS.Interpret (BrowserAudioBuffer, BrowserFloatArray, BrowserPeriodicWave, MediaRecorder)
import WAGS.Run (RunEngine, SceneI, RunAudio)

type Res
  = String

type Assets
  = ( recorders :: { "my-recorder" :: (MediaRecorder -> Effect Unit) }
    , buffers ::
        { "my-buffer" :: BrowserAudioBuffer
        , "shruti" :: BrowserAudioBuffer
        , "reverb" :: BrowserAudioBuffer
        }
    , floatArrays :: { "my-waveshaper" :: BrowserFloatArray }
    , periodicWaves :: { "my-wave" :: BrowserPeriodicWave }
    , analysers :: {}
    , worklets :: {}
    )

type SceneSig :: forall k. k -> Type
type SceneSig proof
  = Scene (SceneI Unit Unit) Assets RunAudio RunEngine proof Res

type FrameSig' step proof a
  = Frame (SceneI Unit Unit) Assets RunAudio RunEngine proof Res step a

type FrameSig step proof
  = FrameSig' step proof { loop :: LoopSig, iteration :: Int }

type WAGSig' step proof a
  = WAG Assets RunAudio RunEngine proof Res step a

type WAGSig step proof
  = WAGSig' step proof { loop :: LoopSig, iteration :: Int }

type IxWAGSig' stepA stepB proof a
  = IxWAG Assets RunAudio RunEngine proof Res stepA stepB a

type IxWAGSig stepA stepB proof
  = IxWAGSig' stepA stepB proof { loop :: LoopSig, iteration :: Int }

type StepSig step proof
  = WAGSig step proof ->
    SceneSig proof

newtype LoopSig
  = LoopSig (forall proof. StepSig StartGraph proof)
