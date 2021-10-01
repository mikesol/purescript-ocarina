module WAGS.Example.KitchenSink.TLP.LoopSig where

import Prelude

import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Scene, WAG)
import WAGS.Example.KitchenSink.Types.StartGraph (StartGraph)
import WAGS.Run (RunEngine, SceneI, RunAudio)
import WAGS.WebAPI (BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

type Res
  = String

type World =
  { recorders :: { "my-recorder" :: MediaRecorderCb }
  , buffers ::
      { "my-buffer" :: BrowserAudioBuffer
      , "shruti" :: BrowserAudioBuffer
      , "reverb" :: BrowserAudioBuffer
      }
  , floatArrays :: { "my-waveshaper" :: BrowserFloatArray }
  , periodicWaves :: { "my-wave" :: BrowserPeriodicWave }
  , microphone :: BrowserMicrophone
  }

type SceneSig :: forall k. k -> Type
type SceneSig proof
  = Scene (SceneI Unit World ()) RunAudio RunEngine proof Res

type FrameSig' step proof a
  = SceneI Unit World () -> WAG RunAudio RunEngine proof Res step a

type FrameSig step proof
  = FrameSig' step proof { loop :: LoopSig, iteration :: Int }

type WAGSig' step proof a
  = WAG RunAudio RunEngine proof Res step a

type WAGSig step proof
  = WAGSig' step proof { loop :: LoopSig, iteration :: Int }

type IxWAGSig' stepA stepB proof a
  = IxWAG RunAudio RunEngine proof Res stepA stepB a

type IxWAGSig stepA stepB proof
  = IxWAGSig' stepA stepB proof { loop :: LoopSig, iteration :: Int }

type StepSig step proof
  =
  WAGSig step proof
  -> SceneSig proof

newtype LoopSig
  = LoopSig (forall proof. StepSig StartGraph proof)
