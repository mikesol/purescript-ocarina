module WAGS.Interpret where

import Prelude

import Control.Bind (bindFlipped)
import Control.Promise (Promise, toAffE)
import Data.ArrayBuffer.Types (ArrayBuffer, Float32Array, Uint8Array)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D1)
import Data.Vec (Vec)
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Random as R
import FRP.Behavior (Behavior, behavior)
import FRP.Event (create, makeEvent, subscribe)
import FRP.Event.Phantom (PhantomEvent, Proof0, proof0, toEvent, unsafePhantom)
import Simple.JSON as JSON
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Control (class ValidateOutputChannelCount)
import WAGS.Core (AudioInterpret(..))
import WAGS.Core as C
import WAGS.Parameter (AudioParameter, WriteHead)
import WAGS.WebAPI (BrowserAudioBuffer)
import WAGS.WebAPI as WebAPI
import Web.File.Blob (Blob)
import Web.File.Url (createObjectURL)

-- foreign non-interpret
data AudioWorkletNodeRequest
  (node :: Symbol)
  (numberOfInputs :: Type)
  (numberOfOutputs :: Type)
  (outputChannelCount :: Type)
  (parameterData :: Row Type)
  (processorOptions :: Row Type) = AudioWorkletNodeRequest

data AudioWorkletNodeResponse
  (node :: Symbol)
  (numberOfInputs :: Type)
  (numberOfOutputs :: Type)
  (outputChannelCount :: Type)
  (parameterData :: Row Type)
  (processorOptions :: Row Type)

foreign import bufferSampleRate :: BrowserAudioBuffer -> Number
foreign import bufferLength :: BrowserAudioBuffer -> Int
foreign import bufferDuration :: BrowserAudioBuffer -> Number
foreign import bufferNumberOfChannels :: BrowserAudioBuffer -> Int
foreign import getFFTSize :: WebAPI.AnalyserNode -> Effect Int

foreign import setFFTSize :: WebAPI.AnalyserNode -> Int -> Effect Unit

foreign import getFrequencyBinCount :: WebAPI.AnalyserNode -> Effect Int

foreign import getSmoothingTimeConstant :: WebAPI.AnalyserNode -> Effect Number

foreign import setSmoothingTimeConstant
  :: WebAPI.AnalyserNode -> Number -> Effect Unit

foreign import getMinDecibels :: WebAPI.AnalyserNode -> Effect Number

foreign import setMinDecibels :: WebAPI.AnalyserNode -> Number -> Effect Unit

foreign import getMaxDecibels :: WebAPI.AnalyserNode -> Effect Number

foreign import setMaxDecibels :: WebAPI.AnalyserNode -> Number -> Effect Unit

foreign import getFloatTimeDomainData
  :: WebAPI.AnalyserNode -> Effect Float32Array

foreign import getFloatFrequencyData
  :: WebAPI.AnalyserNode -> Effect Float32Array

foreign import getByteTimeDomainData :: WebAPI.AnalyserNode -> Effect Uint8Array

foreign import getByteFrequencyData :: WebAPI.AnalyserNode -> Effect Uint8Array

-- | For a given audio context, add the audio worklet module at a given URI.
foreign import audioWorkletAddModule_
  :: WebAPI.AudioContext -> String -> Effect (Promise Unit)

-- | Gets the audio clock time from an audio context.
foreign import getAudioClockTime :: WebAPI.AudioContext -> Effect Number

-- | Stops a media recorder
foreign import stopMediaRecorder :: WebAPI.MediaRecorder -> Effect Unit

-- | For a given MIME type, pass the URL-ified content of a media recorder as a string to a handler.
-- |
-- | ```purescript
-- | mediaRecorderToUrl "audio/ogg" setAudioTagUrlToThisContent recorder
-- | ```
foreign import mediaRecorderToBlob
  :: String -> (Blob -> Effect Unit) -> WebAPI.MediaRecorder -> Effect Unit

mediaRecorderToUrl
  :: String -> (String -> Effect Unit) -> WebAPI.MediaRecorder -> Effect Unit
mediaRecorderToUrl s cb mr = flip (mediaRecorderToBlob s) mr
  (bindFlipped cb <<< createObjectURL)

-- | Is this MIME type supported by this browser.
foreign import isTypeSupported :: String -> Effect Boolean

-- | Given an audio context and a URI, decode the content of the URI to an audio buffer.
decodeAudioDataFromUri
  :: WebAPI.AudioContext -> String -> Aff WebAPI.BrowserAudioBuffer
decodeAudioDataFromUri ctx s =
  toAffE (fetchArrayBuffer s) >>=
    (toAffE <<< decodeAudioDataFromArrayBuffer ctx)

foreign import fetchArrayBuffer :: String -> Effect (Promise ArrayBuffer)

foreign import decodeAudioDataFromArrayBuffer
  :: WebAPI.AudioContext
  -> ArrayBuffer
  -> Effect (Promise WebAPI.BrowserAudioBuffer)

-- | Given an audio context and a base-64-encoded audio file, decode the content of the string to an audio buffer.
foreign import decodeAudioDataFromBase64EncodedString
  :: WebAPI.AudioContext -> String -> Effect (Promise WebAPI.BrowserAudioBuffer)

foreign import makePeriodicWaveImpl
  :: WebAPI.AudioContext
  -> Array Number
  -> Array Number
  -> Effect WebAPI.BrowserPeriodicWave

-- | For a given audio context, use an audio buffer to create a browser audio buffer. This is useful when doing DSP in the browser.  Note that `AudioBuffer` is a purescript type whereas `WebAPI.BrowserAudioBuffer` is an optimized browser-based type. That means that, once you write to `WebAPI.BrowserAudioBuffer`, it is effectively a blob and its contents cannot be retrieved using the WAGS API.
foreign import makeAudioBuffer
  :: WebAPI.AudioContext -> AudioBuffer -> Effect WebAPI.BrowserAudioBuffer

-- | Make a float 32 array. Useful when creating a waveshaper node.
foreign import makeFloatArray :: Array Number -> Effect WebAPI.BrowserFloatArray

-- | Make a new audio context.
foreign import context :: Effect WebAPI.AudioContext

-- | Send 0s from a context immediately. This is useful on iOS so that the context doesn't switch to a suspended state.
foreign import constant0Hack :: WebAPI.AudioContext -> Effect (Effect Unit)

-- | Get the state of the context
foreign import contextState :: WebAPI.AudioContext -> Effect String

-- | Get the state of the context
foreign import contextResume :: WebAPI.AudioContext -> Effect (Promise Unit)

-- | Close an audio context.
foreign import close :: WebAPI.AudioContext -> Effect Unit

foreign import data BrowserMediaStream :: Type

foreign import getBrowserMediaStreamImpl
  :: Boolean -> Boolean -> Effect (Promise BrowserMediaStream)

data Audio

audioWorkletAddModule
  :: forall node numberOfInputs numberOfOutputs outputChannelCount parameterData
       processorOptions
   . IsSymbol node
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData AudioParameter
  => JSON.WriteForeign { | processorOptions }
  => WebAPI.AudioContext
  -> String
  -> ( AudioWorkletNodeRequest node numberOfInputs
         numberOfOutputs
         outputChannelCount
         parameterData
         processorOptions

     )
  -> Aff
       ( AudioWorkletNodeResponse node numberOfInputs
           numberOfOutputs
           outputChannelCount
           parameterData
           processorOptions

       )
audioWorkletAddModule c px _ = toAffE (audioWorkletAddModule_ c px) $>
  unsafeCoerce unit

browserMediaStreamToBrowserMicrophone
  :: BrowserMediaStream -> WebAPI.BrowserMicrophone
browserMediaStreamToBrowserMicrophone = unsafeCoerce

browserMediaStreamToBrowserCamera :: BrowserMediaStream -> WebAPI.BrowserCamera
browserMediaStreamToBrowserCamera = unsafeCoerce

getMicrophoneAndCamera
  :: Boolean
  -> Boolean
  -> Aff
       { microphone :: Maybe WebAPI.BrowserMicrophone
       , camera :: Maybe WebAPI.BrowserCamera
       }
getMicrophoneAndCamera audio video =
  ( \i ->
      { microphone:
          if audio then pure $ browserMediaStreamToBrowserMicrophone i
          else Nothing
      , camera:
          if video then pure $ browserMediaStreamToBrowserCamera i else Nothing
      }
  )
    <$> toAffE (getBrowserMediaStreamImpl audio video)

-- | Create a unit cache. This returns a fresh empty object `{}` that is used to cache audio units.
foreign import makeFFIAudioSnapshot
  :: WebAPI.AudioContext -> Effect FFIAudioSnapshot

foreign import contextFromSnapshot :: FFIAudioSnapshot -> WebAPI.AudioContext
foreign import advanceWriteHead :: FFIAudioSnapshot -> Number -> Effect Unit

-- | Make a browser periodic wave. A PureScript-ified version of the periodic wave constructor
-- | from the [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/PeriodicWave/PeriodicWave).
-- | Given an audio context, a vector of real parts of complex numbers, and a vector of imaginary parts of complex numbers, build a periodic wave interpretable by the Web Audio API.
makePeriodicWave
  :: forall len
   . Lt D1 len
  => WebAPI.AudioContext
  -> Vec len Number
  -> Vec len Number
  -> Effect WebAPI.BrowserPeriodicWave
makePeriodicWave ctx a b = makePeriodicWaveImpl ctx (V.toArray a) (V.toArray b)

-- | A multi-channel audio buffer.
data AudioBuffer = AudioBuffer Int (Array (Array Number))

instance showAudioBuffer :: Show AudioBuffer where
  show (AudioBuffer i a) = "AudioBuffer <" <> show i <> " : " <> show a <> ">"

derive instance eqAudioBuffer :: Eq AudioBuffer

-- | Make a multi-channel audio buffer. Each vector into the multi-channel buffer must be the same length.
audioBuffer
  :: forall bch blen
   . Pos bch
  => Pos blen
  => Int
  -> Vec bch (Vec blen Number)
  -> AudioBuffer
audioBuffer i v = AudioBuffer i (map V.toArray $ V.toArray v)

writeHead :: Number -> WebAPI.AudioContext -> WriteHead Behavior
writeHead lookAhead ctx = behavior \eab -> makeEvent \k ->
  subscribe eab \ab -> do
    t <- getAudioClockTime ctx
    k (ab { concreteTime: t, abstractTime: t - lookAhead, lookAhead })

-- foreign
data FFIAudioSnapshot

foreign import destroyUnit_ :: C.DestroyUnit -> FFIAudioSnapshot -> Effect Unit
foreign import disconnectXFromY_
  :: C.DisconnectXFromY -> FFIAudioSnapshot -> Effect Unit

foreign import connectXToY_ :: C.ConnectXToY -> FFIAudioSnapshot -> Effect Unit
foreign import makeAllpass_ :: C.MakeAllpass -> FFIAudioSnapshot -> Effect Unit
foreign import makeAnalyser_
  :: C.MakeAnalyser -> FFIAudioSnapshot -> Effect Unit

foreign import makeAudioWorkletNode_
  :: C.MakeAudioWorkletNode -> FFIAudioSnapshot -> Effect Unit

foreign import makeBandpass_
  :: C.MakeBandpass -> FFIAudioSnapshot -> Effect Unit

foreign import makeConstant_
  :: C.MakeConstant -> FFIAudioSnapshot -> Effect Unit

foreign import makeConvolver_
  :: C.MakeConvolver -> FFIAudioSnapshot -> Effect Unit

foreign import makeDelay_ :: C.MakeDelay -> FFIAudioSnapshot -> Effect Unit
foreign import makeDynamicsCompressor_
  :: C.MakeDynamicsCompressor -> FFIAudioSnapshot -> Effect Unit

foreign import makeGain_ :: C.MakeGain -> FFIAudioSnapshot -> Effect Unit
foreign import makeHighpass_
  :: C.MakeHighpass -> FFIAudioSnapshot -> Effect Unit

foreign import makeHighshelf_
  :: C.MakeHighshelf -> FFIAudioSnapshot -> Effect Unit

foreign import makeInput_ :: C.MakeInput -> FFIAudioSnapshot -> Effect Unit
foreign import makeLoopBuf_ :: C.MakeLoopBuf -> FFIAudioSnapshot -> Effect Unit
foreign import makeLowpass_ :: C.MakeLowpass -> FFIAudioSnapshot -> Effect Unit
foreign import makeLowshelf_
  :: C.MakeLowshelf -> FFIAudioSnapshot -> Effect Unit

foreign import makeMediaElement_
  :: C.MakeMediaElement -> FFIAudioSnapshot -> Effect Unit

foreign import makeMicrophone_
  :: C.MakeMicrophone -> FFIAudioSnapshot -> Effect Unit

foreign import makeNotch_ :: C.MakeNotch -> FFIAudioSnapshot -> Effect Unit
foreign import makePeaking_ :: C.MakePeaking -> FFIAudioSnapshot -> Effect Unit
foreign import makePeriodicOsc_
  :: C.MakePeriodicOsc -> FFIAudioSnapshot -> Effect Unit

foreign import makePlayBuf_ :: C.MakePlayBuf -> FFIAudioSnapshot -> Effect Unit
foreign import makeRecorder_
  :: C.MakeRecorder -> FFIAudioSnapshot -> Effect Unit

foreign import makeSawtoothOsc_
  :: C.MakeSawtoothOsc -> FFIAudioSnapshot -> Effect Unit

foreign import makeSinOsc_ :: C.MakeSinOsc -> FFIAudioSnapshot -> Effect Unit
foreign import makeSpeaker_ :: C.MakeSpeaker -> FFIAudioSnapshot -> Effect Unit
foreign import makeSquareOsc_
  :: C.MakeSquareOsc -> FFIAudioSnapshot -> Effect Unit

foreign import makeStereoPanner_
  :: C.MakeStereoPanner -> FFIAudioSnapshot -> Effect Unit

foreign import makeSubgraph_
  :: forall index env proof
   . String
  -> String
  -> String
  -> ( index
       -> String
       -> Effect
            { actualized ::
                PhantomEvent proof (FFIAudioSnapshot -> Effect Unit)
            , pusher :: env -> Effect Unit
            }
     )
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeTriangleOsc_
  :: C.MakeTriangleOsc -> FFIAudioSnapshot -> Effect Unit

foreign import makeWaveShaper_
  :: C.MakeWaveShaper -> FFIAudioSnapshot -> Effect Unit

foreign import setAnalyserNodeCb_
  :: C.SetAnalyserNodeCb -> FFIAudioSnapshot -> Effect Unit

foreign import setMediaRecorderCb_
  :: C.SetMediaRecorderCb -> FFIAudioSnapshot -> Effect Unit

foreign import setWaveShaperCurve_
  :: C.SetWaveShaperCurve -> FFIAudioSnapshot -> Effect Unit

foreign import setAudioWorkletParameter_
  :: C.SetAudioWorkletParameter -> FFIAudioSnapshot -> Effect Unit

foreign import setBuffer_ :: C.SetBuffer -> FFIAudioSnapshot -> Effect Unit
foreign import setConvolverBuffer_
  :: C.SetConvolverBuffer -> FFIAudioSnapshot -> Effect Unit

foreign import setPeriodicOsc_
  :: C.SetPeriodicOsc -> FFIAudioSnapshot -> Effect Unit

foreign import setOnOff_ :: C.SetOnOff -> FFIAudioSnapshot -> Effect Unit
foreign import setBufferOffset_
  :: C.SetBufferOffset -> FFIAudioSnapshot -> Effect Unit

foreign import setLoopStart_
  :: C.SetLoopStart -> FFIAudioSnapshot -> Effect Unit

foreign import setLoopEnd_ :: C.SetLoopEnd -> FFIAudioSnapshot -> Effect Unit
foreign import setRatio_ :: C.SetRatio -> FFIAudioSnapshot -> Effect Unit
foreign import setOffset_ :: C.SetOffset -> FFIAudioSnapshot -> Effect Unit
foreign import setGain_ :: C.SetGain -> FFIAudioSnapshot -> Effect Unit
foreign import setQ_ :: C.SetQ -> FFIAudioSnapshot -> Effect Unit
foreign import setFrequency_
  :: C.SetFrequency -> FFIAudioSnapshot -> Effect Unit

foreign import setKnee_ :: C.SetKnee -> FFIAudioSnapshot -> Effect Unit
foreign import setAttack_ :: C.SetAttack -> FFIAudioSnapshot -> Effect Unit
foreign import setRelease_ :: C.SetRelease -> FFIAudioSnapshot -> Effect Unit
foreign import setPan_ :: C.SetPan -> FFIAudioSnapshot -> Effect Unit
foreign import setThreshold_
  :: C.SetThreshold -> FFIAudioSnapshot -> Effect Unit

foreign import setDelay_ :: C.SetDelay -> FFIAudioSnapshot -> Effect Unit
foreign import setPlaybackRate_
  :: C.SetPlaybackRate -> FFIAudioSnapshot -> Effect Unit

foreign import removeSubgraph_
  :: forall index. C.RemoveSubgraph index -> FFIAudioSnapshot -> Effect Unit

foreign import insertOrUpdateSubgraph_
  :: forall index env
   . C.InsertOrUpdateSubgraph index env
  -> FFIAudioSnapshot
  -> Effect Unit

effectfulAudioInterpret
  :: C.AudioInterpret PhantomEvent Proof0 (FFIAudioSnapshot -> Effect Unit)
effectfulAudioInterpret = C.AudioInterpret
  { scope: "root"
  , ids: map show $ behavior \f -> proof0 $ makeEvent \k -> do
      r <- R.random
      subscribe (toEvent f) \x -> k (x r)
  , destroyUnit: destroyUnit_
  , disconnectXFromY: disconnectXFromY_
  , connectXToY: connectXToY_
  , makeAllpass: makeAllpass_
  , makeAnalyser: makeAnalyser_
  , makeAudioWorkletNode: makeAudioWorkletNode_
  , makeBandpass: makeBandpass_
  , makeConstant: makeConstant_
  , makeConvolver: makeConvolver_
  , makeDelay: makeDelay_
  , makeDynamicsCompressor: makeDynamicsCompressor_
  , makeGain: makeGain_
  , makeHighpass: makeHighpass_
  , makeHighshelf: makeHighshelf_
  , makeInput: makeInput_
  , makeLoopBuf: makeLoopBuf_
  , makeLowpass: makeLowpass_
  , makeLowshelf: makeLowshelf_
  , makeMediaElement: makeMediaElement_
  , makeMicrophone: makeMicrophone_
  , makeNotch: makeNotch_
  , makePeaking: makePeaking_
  , makePeriodicOsc: makePeriodicOsc_
  , makePlayBuf: makePlayBuf_
  , makeRecorder: makeRecorder_
  , makeSawtoothOsc: makeSawtoothOsc_
  , makeSinOsc: makeSinOsc_
  , makeSpeaker: makeSpeaker_
  , makeSquareOsc: makeSquareOsc_
  , makeStereoPanner: makeStereoPanner_
  , makeSubgraph: \{ id, parent, scope, scenes } audio ->
      flip (makeSubgraph_ id parent scope) audio \index newScope ->
        do
          evt <- create
          let event = evt.event
          let
            actualized =
              let
                C.Node elt = (let C.Subgraph sg = scenes in sg) index (unsafePhantom event)
              in
                elt id
                  ( let
                      AudioInterpret ai = effectfulAudioInterpret
                    in
                      AudioInterpret ai { scope = newScope }
                  )
          pure { actualized, pusher: evt.push }
  , makeTriangleOsc: makeTriangleOsc_
  , makeTumult: \_ _ -> pure unit -- todo: makeTumult_
  , makeWaveShaper: makeWaveShaper_
  , setAnalyserNodeCb: setAnalyserNodeCb_
  , setMediaRecorderCb: setMediaRecorderCb_
  , setWaveShaperCurve: setWaveShaperCurve_
  , setAudioWorkletParameter: setAudioWorkletParameter_
  , setBuffer: setBuffer_
  , setConvolverBuffer: setConvolverBuffer_
  , setPeriodicOsc: setPeriodicOsc_
  , setOnOff: setOnOff_
  , setBufferOffset: setBufferOffset_
  , setLoopStart: setLoopStart_
  , setLoopEnd: setLoopEnd_
  , setRatio: setRatio_
  , setOffset: setOffset_
  , setAttack: setAttack_
  , setGain: setGain_
  , setQ: setQ_
  , setPan: setPan_
  , setThreshold: setThreshold_
  , setRelease: setRelease_
  , setKnee: setKnee_
  , setDelay: setDelay_
  , setPlaybackRate: setPlaybackRate_
  , setFrequency: setFrequency_
  , removeSubgraph: removeSubgraph_
  , insertOrUpdateSubgraph: insertOrUpdateSubgraph_
  , setTumult: \_ _ -> pure unit -- todo: setTumult_
  }
