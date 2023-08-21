module Ocarina.WebAPI where

import Prelude

import Data.Function (on)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Unsafe.Reference (unsafeRefEq)

-- | An [AnalyserNode](https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode)
foreign import data AnalyserNode :: Type

instance showAnalyserNode :: Show AnalyserNode where
  show = const "<AnalyserNode>"

instance eqAnalyserNode :: Eq AnalyserNode where
  eq = unsafeRefEq

newtype AnalyserNodeCb = AnalyserNodeCb (AnalyserNode -> Effect (Effect Unit))

derive instance newtypeAnalyserNodeCb :: Newtype AnalyserNodeCb _
instance showAnalyserNodeCb :: Show AnalyserNodeCb where
  show = const "<AnalyserNodeCb>"

instance eqAnalyserNodeCb :: Eq AnalyserNodeCb where
  eq = unsafeRefEq `on` unwrap

instance ordAnalyserNodeCb :: Ord AnalyserNodeCb where
  compare a b = if a == b then EQ else LT

-- | An [AudioWorkletNode](https://developer.mozilla.org/en-US/docs/Web/API/AudioWorkletNode)
foreign import data AudioWorkletNode :: Symbol -> Row Type -> Type

instance showAudioWorkletNode :: Show (AudioWorkletNode sym row) where
  show = const "<AudioWorkletNode>"

instance eqAudioWorkletNode :: Eq (AudioWorkletNode sym row) where
  eq = unsafeRefEq

-- | A [MediaRecorder](https://developer.mozilla.org/en-US/docs/Web/API/MediaRecorder).
foreign import data MediaRecorder :: Type

instance showMediaRecorder :: Show MediaRecorder where
  show = const "<MediaRecorder>"

instance eqMediaRecorder :: Eq MediaRecorder where
  eq = unsafeRefEq

newtype MediaRecorderCb = MediaRecorderCb (MediaRecorder -> Effect Unit)

derive instance newtypeMediaRecorderCb :: Newtype MediaRecorderCb _
instance showMediaRecorderCb :: Show MediaRecorderCb where
  show = const "<MediaRecorderCb>"

instance eqMediaRecorderCb :: Eq MediaRecorderCb where
  eq = unsafeRefEq `on` unwrap

instance ordMediaRecorderCb :: Ord MediaRecorderCb where
  compare a b = if a == b then EQ else LT

-- | A [PeriodicWave](https://developer.mozilla.org/en-US/docs/Web/API/PeriodicWave).
foreign import data BrowserPeriodicWave :: Type

instance showBrowserPeriodicWave :: Show BrowserPeriodicWave where
  show = const "<BrowserPeriodicWave>"

instance eqBrowserPeriodicWave :: Eq BrowserPeriodicWave where
  eq = unsafeRefEq

instance ordBrowserPeriodicWave :: Ord BrowserPeriodicWave where
  compare a b = if a == b then EQ else LT

-- | An [AudioBuffer](https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer).
foreign import data BrowserAudioBuffer :: Type

instance showBrowserAudioBuffer :: Show BrowserAudioBuffer where
  show = const "<BrowserAudioBuffer>"

instance eqBrowserAudioBuffer :: Eq BrowserAudioBuffer where
  eq = unsafeRefEq

instance ordBrowserAudioBuffer :: Ord BrowserAudioBuffer where
  compare a b = if a == b then EQ else LT

-- | A [Float32Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array)
foreign import data BrowserFloatArray :: Type

instance showBrowserFloatArray :: Show BrowserFloatArray where
  show = const "<BrowserFloatArray>"

instance eqBrowserFloatArray :: Eq BrowserFloatArray where
  eq = unsafeRefEq

instance ordBrowserFloatArray :: Ord BrowserFloatArray where
  compare a b = if a == b then EQ else LT

-- | An [AudioContext](https://developer.mozilla.org/en-US/docs/Web/API/AudioContext)
foreign import data AudioContext :: Type

instance showAudioContext :: Show AudioContext where
  show = const "<AudioContext>"

instance eqAudioContext :: Eq AudioContext where
  eq = unsafeRefEq

-- | The [MediaStream](https://developer.mozilla.org/en-US/docs/Web/API/MediaStream) object for a microphone.
foreign import data BrowserMicrophone :: Type

instance showBrowserMicrophone :: Show BrowserMicrophone where
  show = const "<BrowserMicrophone>"

instance eqBrowserMicrophone :: Eq BrowserMicrophone where
  eq = unsafeRefEq

instance ordBrowserMicrophone :: Ord BrowserMicrophone where
  compare = const $ const EQ

-- | The [MediaStream](https://developer.mozilla.org/en-US/docs/Web/API/MediaStream) object for a camera.
foreign import data BrowserCamera :: Type

instance showBrowserCamera :: Show BrowserCamera where
  show = const "<BrowserCamera>"

instance eqBrowserCamera :: Eq BrowserCamera where
  eq = unsafeRefEq

-- | An [HTMLMediaElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement).
foreign import data BrowserMediaElement :: Type

instance showBrowserMediaElement :: Show BrowserMediaElement where
  show = const "<BrowserMediaElement>"

instance eqBrowserMediaElement :: Eq BrowserMediaElement where
  eq = unsafeRefEq

instance ordBrowserMediaElement :: Ord BrowserMediaElement where
  compare a b = if a == b then EQ else LT
