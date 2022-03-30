-- | Interpret audio in an engine using the `AudioInterpret` class.
-- |
-- | WAGS ships with two engines - `Effect Unit` for real audio and `Instruction` for an ADT representation of audio.
module WAGS.Interpret
  ( class AudioInterpret
  , AudioBuffer
  , FFIAudioSnapshot
  , makeFFIAudioSnapshot
  , contextFromSnapshot
  , advanceWriteHead
  , audioBuffer
  , audioWorkletAddModule
  , close
  , constant0Hack
  , connectXToY
  , context
  , contextState
  , contextResume
  , decodeAudioDataFromBase64EncodedString
  , decodeAudioDataFromUri
  , fetchArrayBuffer
  , decodeAudioDataFromArrayBuffer
  , destroyUnit
  , disconnectXFromY
  , getAudioClockTime
  , getMicrophoneAndCamera
  , isTypeSupported
  , getFFTSize
  , setFFTSize
  , getFrequencyBinCount
  , getSmoothingTimeConstant
  , setSmoothingTimeConstant
  , getMinDecibels
  , setMinDecibels
  , getMaxDecibels
  , setMaxDecibels
  , getFloatTimeDomainData
  , getFloatFrequencyData
  , getByteTimeDomainData
  , getByteFrequencyData
  , makeAllpass
  , makeAnalyser
  , makeAudioBuffer
  , makeAudioWorkletNode
  , makeBandpass
  , makeConstant
  , makePassthroughConvolver
  , makeConvolver
  , makeDelay
  , makeDynamicsCompressor
  , makeFloatArray
  , makeGain
  , makeHighpass
  , makeHighshelf
  , makeLoopBuf
  , makeLoopBufWithDeferredBuffer
  , makeLowpass
  , makeLowshelf
  , makeMediaElement
  , makeMicrophone
  , makeNotch
  , makePeaking
  , makePeriodicOsc
  , makePeriodicOscV
  , makePeriodicOscWithDeferredOsc
  , makePeriodicWave
  , makePlayBuf
  , makePlayBufWithDeferredBuffer
  , makeRecorder
  , makeSawtoothOsc
  , makeSinOsc
  , makeSpeaker
  , makeSquareOsc
  , makeStereoPanner
  , makeTriangleOsc
  , makeWaveShaper
  , mediaRecorderToUrl
  , mediaRecorderToBlob
  , makeInput
  , makeTumult
  , setInput
  , renderAudio
  , setAnalyserNodeCb
  , setMediaRecorderCb
  , setAudioWorkletParameter
  , setBuffer
  , setConvolverBuffer
  , setPeriodicOsc
  , setPeriodicOscV
  , setAttack
  , setDelay
  , setFrequency
  , setGain
  , setKnee
  , setBufferOffset
  , setDuration
  , setLoopEnd
  , setLoopStart
  , setOffset
  , setOnOff
  , setPan
  , setPlaybackRate
  , setQ
  , setRatio
  , setRelease
  , setThreshold
  , setWaveShaperCurve
  , stopMediaRecorder
  , bufferSampleRate
  , bufferLength
  , bufferDuration
  , bufferNumberOfChannels
  , makeSubgraph
  , setSubgraph
  , setTumult
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Plus (empty)
import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.ArrayBuffer.Types (Float32Array, Uint8Array, ArrayBuffer)
import Data.Compactable (compact)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toNullable)
import Data.Set as Set
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D1)
import Data.Typelevel.Undefined (undefined)
import Data.Variant (match)
import Data.Variant.Maybe as VM
import Data.Vec (Vec)
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (Aff)
import Simple.JSON as JSON
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Control.Types (Frame0, SubScene(..), oneSubFrame)
import WAGS.Graph.Paramable (paramize)
import WAGS.Graph.Parameter (AudioOnOff, AudioParameter)
import WAGS.Graph.Worklet (AudioWorkletNodeRequest, AudioWorkletNodeResponse)
import WAGS.Rendered (Instruction, RealImg)
import WAGS.Rendered as R
import WAGS.Tumult.Reconciliation (reconcileTumult)
import WAGS.Util (class ValidateOutputChannelCount)
import WAGS.WebAPI (BrowserAudioBuffer, BrowserPeriodicWave)
import WAGS.WebAPI as WebAPI
import Web.File.Blob (Blob)
import Web.File.Url (createObjectURL)

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
          else empty
      , camera:
          if video then pure $ browserMediaStreamToBrowserCamera i else empty
      }
  )
    <$> toAffE (getBrowserMediaStreamImpl audio video)

-- | Create a unit cache. This returns a fresh empty object `{}` that is used to cache audio units.
foreign import makeFFIAudioSnapshot
  :: WebAPI.AudioContext -> Effect FFIAudioSnapshot

foreign import contextFromSnapshot :: FFIAudioSnapshot -> WebAPI.AudioContext
foreign import advanceWriteHead :: FFIAudioSnapshot -> Number -> Effect Unit

-- | Render audio from an array of audio rendering instructions. This is conceptually the same as
-- | taking `Array Effect Unit -> Effect Unit` and doing `map fold <<< sequence`.
-- | The reason this version is used is because it is ~2x more computationally efficient,
-- | which is important in order to be able to hit audio deadlines.
foreign import renderAudio :: Array (Effect Unit) -> Effect Unit

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

foreign import bufferSampleRate :: BrowserAudioBuffer -> Number
foreign import bufferLength :: BrowserAudioBuffer -> Int
foreign import bufferDuration :: BrowserAudioBuffer -> Number
foreign import bufferNumberOfChannels :: BrowserAudioBuffer -> Int

data FFIAudioSnapshot
type Ie index env =
  { pos :: Int
  , index :: index
  , env :: VM.Maybe env
  }

type SubgraphInput
  :: (Symbol -> Type)
  -> Symbol
  -> Row Type
  -> Type
  -> Type
  -> Type
  -> Type
  -> Type
type SubgraphInput proxy terminus inputs index env audio engine =
  { id :: String
  , terminus :: proxy terminus
  , envs :: Array (Ie index env)
  , scenes ::
      index -> SubScene terminus inputs env audio engine Frame0 Unit
  }

type SetSubgraphInput index env =
  { id :: String
  , envs :: Array (Ie index env)
  }

type MakePeriodicOscW =
  { id :: String
  , wave :: BrowserPeriodicWave
  , onOff :: AudioOnOff
  , freq :: AudioParameter
  }

type SetPeriodicOscW =
  { id :: String
  , wave :: BrowserPeriodicWave
  }

type MakePeriodicOscV =
  { id :: String
  , realImg :: RealImg
  , onOff :: AudioOnOff
  , freq :: AudioParameter
  }

type SetPeriodicOscV =
  { id :: String
  , realImg :: RealImg
  }

-- | A class with all possible instructions for interpreting audio.
-- | The class is paramaterized by two types:
-- | - `audio`: an audio context, which could be nothing (ie `Unit`) if there is audio or `FFIAudio` if there is audio.
-- | - `engine`: the output of the engine. For real audio, this is `Effect Unit`, as playing something from a loudspeaker is a side effect that doesn't return anything. For testing, this is the `Instruction` type, which is an ADT representation of instructions to an audio engine.
class AudioInterpret audio engine where
  -- | Connect pointer x to pointer y. For example, connect a sine wave oscillator to a highpass filter.
  connectXToY :: R.ConnectXToY -> audio -> engine
  -- | Disconnect pointer x from pointer y. For example, disconnect a sine wave oscillator from a gain unit.
  disconnectXFromY :: R.DisconnectXFromY -> audio -> engine
  -- | Destroy pointer x. For example, drop a sine wave oscillator from an audio graph. Note that this does not invoke garbage collection - it just removes the reference to the node, allowing it to be garbage collected.
  destroyUnit :: R.DestroyUnit -> audio -> engine
  -- | Make an allpass filter.
  makeAllpass :: R.MakeAllpass -> audio -> engine
  -- | Make an analyser.
  makeAnalyser :: R.MakeAnalyser -> audio -> engine
  -- | Make an audio worklet node.
  makeAudioWorkletNode :: R.MakeAudioWorkletNode -> audio -> engine
  -- | Make a bandpass filter.
  makeBandpass :: R.MakeBandpass -> audio -> engine
  -- | Make a constant source, ie a stream of 0s.
  makeConstant :: R.MakeConstant -> audio -> engine
  -- | Make a "passthrough" convolver where the buffer will be set later.
  makePassthroughConvolver :: R.MakePassthroughConvolver -> audio -> engine
  -- | Make a convolution unit, aka reverb.
  makeConvolver :: R.MakeConvolver -> audio -> engine
  -- | Make a delay unit.
  makeDelay :: R.MakeDelay -> audio -> engine
  -- | Make a compressor/expander.
  makeDynamicsCompressor :: R.MakeDynamicsCompressor -> audio -> engine
  -- | Make a gain unit.
  makeGain :: R.MakeGain -> audio -> engine
  -- | Make a highpass filter.
  makeHighpass :: R.MakeHighpass -> audio -> engine
  -- | Make a highshelf filter.
  makeHighshelf :: R.MakeHighshelf -> audio -> engine
  -- | Make input.
  makeInput :: R.MakeInput -> audio -> engine
  -- | Make a looping audio buffer node with a deferred buffer.
  makeLoopBufWithDeferredBuffer
    :: R.MakeLoopBufWithDeferredBuffer -> audio -> engine
  -- | Make a looping audio buffer node.
  makeLoopBuf :: R.MakeLoopBuf -> audio -> engine
  -- | Make a lowpass filter.
  makeLowpass :: R.MakeLowpass -> audio -> engine
  -- | Make a lowshelf filter.
  makeLowshelf :: R.MakeLowshelf -> audio -> engine
  -- | Make a media element.
  makeMediaElement :: R.MakeMediaElement -> audio -> engine
  -- | Make a microphone.
  makeMicrophone :: R.MakeMicrophone -> audio -> engine
  -- | Make a notch filter.
  makeNotch :: R.MakeNotch -> audio -> engine
  -- | Make a peaking filter.
  makePeaking :: R.MakePeaking -> audio -> engine
  -- | Make a periodic oscillator.
  makePeriodicOscWithDeferredOsc
    :: R.MakePeriodicOscWithDeferredOsc -> audio -> engine
  -- | Make a periodic oscillator.
  makePeriodicOsc :: MakePeriodicOscW -> audio -> engine
  -- | Make a periodic oscillator
  makePeriodicOscV :: MakePeriodicOscV -> audio -> engine
  -- | Make an audio buffer node with a deferred buffer.
  makePlayBufWithDeferredBuffer
    :: R.MakePlayBufWithDeferredBuffer -> audio -> engine
  -- | Make an audio buffer node.
  makePlayBuf :: R.MakePlayBuf -> audio -> engine
  -- | Make a recorder.
  makeRecorder :: R.MakeRecorder -> audio -> engine
  -- | Make a sawtooth oscillator.
  makeSawtoothOsc :: R.MakeSawtoothOsc -> audio -> engine
  -- | Make a sine-wave oscillator.
  makeSinOsc :: R.MakeSinOsc -> audio -> engine
  -- | Make a node representing the loudspeaker. For sound to be rendered, it must go to a loudspeaker.
  makeSpeaker :: audio -> engine
  -- | Make a square-wave oscillator.
  makeSquareOsc :: R.MakeSquareOsc -> audio -> engine
  -- | Make a stereo panner
  makeStereoPanner :: R.MakeStereoPanner -> audio -> engine
  -- | Make sugbraph.
  makeSubgraph
    :: forall proxy terminus inputs index env
     . IsSymbol terminus
    => SubgraphInput proxy terminus inputs index env audio engine
    -> audio
    -> engine
  -- | Make a triangle-wave oscillator.
  makeTriangleOsc :: R.MakeTriangleOsc -> audio -> engine
  -- | Make tumult.
  makeTumult :: R.MakeTumult -> audio -> engine
  -- | Make a wave shaper.
  makeWaveShaper :: R.MakeWaveShaper -> audio -> engine
  -- | Sets the callback used by an analyser node
  setAnalyserNodeCb :: R.SetAnalyserNodeCb -> audio -> engine
  -- | Sets the callback used by a recorder node
  setMediaRecorderCb :: R.SetMediaRecorderCb -> audio -> engine
  -- | Sets the waveshaper curve float array
  setWaveShaperCurve :: R.SetWaveShaperCurve -> audio -> engine
  -- | Sets a custom parameter for an audio worklet node
  setAudioWorkletParameter :: R.SetAudioWorkletParameter -> audio -> engine
  -- | Sets the buffer to read from in a playBuf or loopBuf
  setBuffer :: R.SetBuffer -> audio -> engine
  -- | Sets the buffer to read from in a convolver
  setConvolverBuffer :: R.SetConvolverBuffer -> audio -> engine
  -- | Sets the periodic oscillator to read from in a periodicOsc
  setPeriodicOsc :: SetPeriodicOscW -> audio -> engine
  -- | Sets the periodic oscillator to read from in a periodicOsc
  setPeriodicOscV :: SetPeriodicOscV -> audio -> engine
  -- | Turn on or off a generator (an oscillator or playback node).
  setOnOff :: R.SetOnOff -> audio -> engine
  -- | Set the offset for a playbuf
  setBufferOffset :: R.SetBufferOffset -> audio -> engine
  -- | Set the duration for a playbuf
  setDuration :: R.SetDuration -> audio -> engine
  -- | Set the start position of a looping audio buffer node.
  setLoopStart :: R.SetLoopStart -> audio -> engine
  -- | Set the end position of a looping audio buffer node.
  setLoopEnd :: R.SetLoopEnd -> audio -> engine
  -- | Set the ratio of a compressor.
  setRatio :: R.SetRatio -> audio -> engine
  -- | Set the offset of a constant source node.
  setOffset :: R.SetOffset -> audio -> engine
  -- | Set the attack of a compressor.
  setAttack :: R.SetAttack -> audio -> engine
  -- | Set the gain of a gain node or filter.
  setGain :: R.SetGain -> audio -> engine
  -- | Set the q of a biquad filter.
  setQ :: R.SetQ -> audio -> engine
  -- | Set the pan of a stereo panner.
  setPan :: R.SetPan -> audio -> engine
  -- | Set the threshold of a compressor.
  setThreshold :: R.SetThreshold -> audio -> engine
  -- | Set the release of a compressor.
  setRelease :: R.SetRelease -> audio -> engine
  -- | Set the knee of a compressor.
  setKnee :: R.SetKnee -> audio -> engine
  -- | Set the delay of a delay node.
  setDelay :: R.SetDelay -> audio -> engine
  -- | Set the playback rate of an audio node buffer or loop buffer.
  setPlaybackRate :: R.SetPlaybackRate -> audio -> engine
  -- | Set the frequency of an oscillator or filter.
  setFrequency :: R.SetFrequency -> audio -> engine
  -- | Set input
  setInput :: R.SetInput -> audio -> engine
  -- | Set subgraph.
  setSubgraph
    :: forall index env
     . SetSubgraphInput index env
    -> audio
    -> engine
  setTumult :: R.SetTumult -> audio -> engine

handleSubgraph
  :: forall proxy terminus inputs env n
   . (R.MakeSubgraph -> Instruction)
  -> SubgraphInput proxy terminus inputs n env Unit Instruction
  -> Unit
  -> Instruction
handleSubgraph f { id, envs, scenes } = const $ f
  { id
  , instructions:
      defer \_ ->
        let
          subs = map
            ( \{ index, env } -> VM.maybe Nothing Just $ map
                (oneSubFrame (scenes index))
                env
            )
            envs
        in
          (map <<< map) ((#) unit) (map _.instructions (compact subs))
  }

instance freeAudioInterpret :: AudioInterpret Unit Instruction where
  connectXToY = const <<< R.iConnectXToY
  disconnectXFromY = const <<< R.iDisconnectXFromY
  destroyUnit = const <<< R.iDestroyUnit
  makeAllpass = const <<< R.iMakeAllpass
  makeAnalyser = const <<< R.iMakeAnalyser
  makeAudioWorkletNode = const <<< R.iMakeAudioWorkletNode
  makeBandpass = const <<< R.iMakeBandpass
  makeConstant = const <<< R.iMakeConstant
  makePassthroughConvolver = const <<< R.iMakePassthroughConvolver
  makeConvolver = const <<< R.iMakeConvolver
  makeDelay = const <<< R.iMakeDelay
  makeInput = const <<< R.iMakeInput
  makeTumult = const <<< R.iMakeTumult
  makeSubgraph = handleSubgraph R.iMakeSubgraph
  makeDynamicsCompressor = const <<< R.iMakeDynamicsCompressor
  makeGain = const <<< R.iMakeGain
  makeHighpass = const <<< R.iMakeHighpass
  makeHighshelf = const <<< R.iMakeHighshelf
  makeLoopBufWithDeferredBuffer = const <<< R.iMakeLoopBufWithDeferredBuffer
  makeLoopBuf = const <<< R.iMakeLoopBuf
  makeLowpass = const <<< R.iMakeLowpass
  makeLowshelf = const <<< R.iMakeLowshelf
  makeMediaElement = const <<< R.iMakeMediaElement
  makeMicrophone = const <<< R.iMakeMicrophone
  makeNotch = const <<< R.iMakeNotch
  makePeaking = const <<< R.iMakePeaking
  makePeriodicOsc { id, onOff, wave, freq } = const $ R.iMakePeriodicOsc
    { id, onOff, spec: R.iWave wave, freq }
  makePeriodicOscV { id, onOff, realImg, freq } = const $ R.iMakePeriodicOsc
    { id, onOff, spec: R.iRealImg realImg, freq }
  makePeriodicOscWithDeferredOsc = const <<< R.iMakePeriodicOscWithDeferredOsc
  makePlayBufWithDeferredBuffer = const <<< R.iMakePlayBufWithDeferredBuffer
  makePlayBuf = const <<< R.iMakePlayBuf
  makeRecorder = const <<< R.iMakeRecorder
  makeSawtoothOsc = const <<< R.iMakeSawtoothOsc
  makeSinOsc = const <<< R.iMakeSinOsc
  makeSpeaker = const R.iMakeSpeaker
  makeSquareOsc = const <<< R.iMakeSquareOsc
  makeStereoPanner = const <<< R.iMakeStereoPanner
  makeTriangleOsc = const <<< R.iMakeTriangleOsc
  makeWaveShaper = const <<< R.iMakeWaveShaper
  setAudioWorkletParameter = const <<< R.iSetAudioWorkletParameter
  setBuffer = const <<< R.iSetBuffer
  setConvolverBuffer = const <<< R.iSetConvolverBuffer
  setPeriodicOsc { id, wave } = const $ R.iSetPeriodicOsc
    { id, periodicOsc: R.iWave wave }
  setPeriodicOscV { id, realImg } = const $ R.iSetPeriodicOsc
    { id, periodicOsc: R.iRealImg realImg }
  setOnOff = const <<< R.iSetOnOff
  setMediaRecorderCb = const <<< R.iSetMediaRecorderCb
  setAnalyserNodeCb = const <<< R.iSetAnalyserNodeCb
  setBufferOffset = const <<< R.iSetBufferOffset
  setDuration = const <<< R.iSetDuration
  setLoopStart = const <<< R.iSetLoopStart
  setLoopEnd = const <<< R.iSetLoopEnd
  setRatio = const <<< R.iSetRatio
  setOffset = const <<< R.iSetOffset
  setAttack = const <<< R.iSetAttack
  setGain = const <<< R.iSetGain
  setQ = const <<< R.iSetQ
  setPan = const <<< R.iSetPan
  setThreshold = const <<< R.iSetThreshold
  setRelease = const <<< R.iSetRelease
  setKnee = const <<< R.iSetKnee
  setDelay = const <<< R.iSetDelay
  setPlaybackRate = const <<< R.iSetPlaybackRate
  setFrequency = const <<< R.iSetFrequency
  setWaveShaperCurve = const <<< R.iSetWaveShaperCurve
  setInput = const <<< R.iSetInput
  setSubgraph { id } _ = R.iSetSubgraph { id }
  setTumult = const <<< R.iSetTumult

foreign import connectXToY_
  :: forall r
   . { fromId :: String, toId :: String | r }
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import disconnectXFromY_
  :: forall r
   . { fromId :: String, toId :: String | r }
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import destroyUnit_
  :: forall r. { id :: String | r } -> FFIAudioSnapshot -> Effect Unit

foreign import makeAllpass_
  :: R.MakeAllpass
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeAnalyser_
  :: R.MakeAnalyser
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeAudioWorkletNode_
  :: R.MakeAudioWorkletNode -> FFIAudioSnapshot -> Effect Unit

foreign import makeBandpass_
  :: R.MakeBandpass
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeConstant_
  :: R.MakeConstant
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeConvolver_
  :: R.MakeConvolver -> FFIAudioSnapshot -> Effect Unit

foreign import makePassthroughConvolver_
  :: { id :: String } -> FFIAudioSnapshot -> Effect Unit

foreign import makeDelay_
  :: R.MakeDelay -> FFIAudioSnapshot -> Effect Unit

foreign import makeDynamicsCompressor_
  :: R.MakeDynamicsCompressor
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeGain_
  :: R.MakeGain -> FFIAudioSnapshot -> Effect Unit

foreign import makeHighpass_
  :: R.MakeHighpass
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeHighshelf_
  :: R.MakeHighshelf
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeLoopBufWithDeferredBuffer_
  :: { id :: String } -> FFIAudioSnapshot -> Effect Unit

foreign import makeLoopBuf_
  :: R.MakeLoopBuf
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeLowpass_
  :: R.MakeLowpass
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeLowshelf_
  :: R.MakeLowshelf
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeMediaElement_
  :: R.MakeMediaElement -> FFIAudioSnapshot -> Effect Unit

foreign import makeMicrophone_
  :: R.MakeMicrophone -> FFIAudioSnapshot -> Effect Unit

foreign import makeNotch_
  :: R.MakeNotch
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makePeaking_
  :: R.MakePeaking
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makePeriodicOscWithDeferredOsc_
  :: { id :: String } -> FFIAudioSnapshot -> Effect Unit

foreign import makePeriodicOsc_
  :: MakePeriodicOscW
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makePeriodicOscV_
  :: MakePeriodicOscV
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makePlayBufWithDeferredBuffer_
  :: { id :: String } -> FFIAudioSnapshot -> Effect Unit

foreign import makePlayBuf_
  :: R.MakePlayBuf
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeRecorder_
  :: R.MakeRecorder -> FFIAudioSnapshot -> Effect Unit

foreign import makeSawtoothOsc_
  :: R.MakeSawtoothOsc
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeSinOsc_
  :: R.MakeSinOsc
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeSpeaker_ :: FFIAudioSnapshot -> Effect Unit

foreign import makeSquareOsc_
  :: R.MakeSquareOsc
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeStereoPanner_
  :: R.MakeStereoPanner -> FFIAudioSnapshot -> Effect Unit

foreign import makeTriangleOsc_
  :: R.MakeTriangleOsc
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeWaveShaper_
  :: R.MakeWaveShaper
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeInput_ :: R.MakeInput -> FFIAudioSnapshot -> Effect Unit

type Pie index env =
  { pos :: Int, index :: index, env :: Nullable env }

foreign import makeSubgraph_
  :: forall index env scene
   . String
  -> String
  -> Array (Pie index env)
  -> (index -> scene)
  -> ( env
       -> scene
       -> { instructions :: Array (FFIAudioSnapshot -> Effect Unit)
          , nextScene :: scene
          }
     )
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import makeTumult_
  :: String
  -> String
  -> Array (Array Instruction)
  -> Maybe (Array Instruction)
  -> (Array Instruction -> Maybe (Array Instruction))
  -> ( Array Instruction
       -> Maybe (Array Instruction)
       -> Array (FFIAudioSnapshot -> Effect Unit)
     )
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import setTumult_
  :: String
  -> String
  -> Array (Array Instruction)
  -> Maybe (Array Instruction)
  -> (Array Instruction -> Maybe (Array Instruction))
  -> ( Array Instruction
       -> Maybe (Array Instruction)
       -> Array (FFIAudioSnapshot -> Effect Unit)
     )
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import setAudioWorkletParameter_
  :: R.SetAudioWorkletParameter -> FFIAudioSnapshot -> Effect Unit

foreign import setOnOff_
  :: R.SetOnOff -> FFIAudioSnapshot -> Effect Unit

foreign import setBufferOffset_
  :: R.SetBufferOffset -> FFIAudioSnapshot -> Effect Unit

foreign import setDuration_
  :: R.SetDuration -> FFIAudioSnapshot -> Effect Unit

foreign import setLoopStart_
  :: R.SetLoopStart -> FFIAudioSnapshot -> Effect Unit

foreign import setLoopEnd_
  :: R.SetLoopEnd -> FFIAudioSnapshot -> Effect Unit

foreign import setRatio_
  :: R.SetRatio -> FFIAudioSnapshot -> Effect Unit

foreign import setOffset_
  :: R.SetOffset -> FFIAudioSnapshot -> Effect Unit

foreign import setAttack_
  :: R.SetAttack -> FFIAudioSnapshot -> Effect Unit

foreign import setGain_
  :: R.SetGain -> FFIAudioSnapshot -> Effect Unit

foreign import setQ_
  :: R.SetQ -> FFIAudioSnapshot -> Effect Unit

foreign import setPan_
  :: R.SetPan -> FFIAudioSnapshot -> Effect Unit

foreign import setThreshold_
  :: R.SetThreshold -> FFIAudioSnapshot -> Effect Unit

foreign import setRelease_
  :: R.SetRelease -> FFIAudioSnapshot -> Effect Unit

foreign import setKnee_
  :: R.SetKnee -> FFIAudioSnapshot -> Effect Unit

foreign import setDelay_
  :: R.SetDelay -> FFIAudioSnapshot -> Effect Unit

foreign import setPlaybackRate_
  :: R.SetPlaybackRate -> FFIAudioSnapshot -> Effect Unit

foreign import setFrequency_
  :: R.SetFrequency -> FFIAudioSnapshot -> Effect Unit

foreign import setBuffer_
  :: R.SetBuffer -> FFIAudioSnapshot -> Effect Unit

foreign import setConvolverBuffer_
  :: R.SetConvolverBuffer -> FFIAudioSnapshot -> Effect Unit

foreign import setPeriodicOsc_
  :: SetPeriodicOscW -> FFIAudioSnapshot -> Effect Unit

foreign import setPeriodicOscV_
  :: SetPeriodicOscV -> FFIAudioSnapshot -> Effect Unit

foreign import setAnalyserNodeCb_
  :: R.SetAnalyserNodeCb
  -> FFIAudioSnapshot
  -> Effect Unit

foreign import setMediaRecorderCb_
  :: R.SetMediaRecorderCb -> FFIAudioSnapshot -> Effect Unit

foreign import setWaveShaperCurve_
  :: R.SetWaveShaperCurve -> FFIAudioSnapshot -> Effect Unit

foreign import setInput_ :: R.SetInput -> FFIAudioSnapshot -> Effect Unit

foreign import setSubgraph_
  :: forall index env
   . String
  -> Array (Pie index env)
  -> FFIAudioSnapshot
  -> Effect Unit

interpretInstruction
  :: forall audio engine
   . AudioInterpret audio engine
  => Instruction
  -> audio
  -> engine
interpretInstruction = unwrap >>> match
  { disconnectXFromY: \a -> disconnectXFromY a
  , destroyUnit: \a -> destroyUnit a
  , makeAllpass: \a -> makeAllpass a
  , makeAnalyser: \a -> makeAnalyser a
  , makeAudioWorkletNode: \a -> makeAudioWorkletNode a
  , makeBandpass: \a -> makeBandpass a
  , makeConstant: \a -> makeConstant a
  , makePassthroughConvolver: \a -> makePassthroughConvolver a
  , makeConvolver: \a -> makeConvolver a
  , makeDelay: \a -> makeDelay a
  , makeDynamicsCompressor: \a -> makeDynamicsCompressor a
  , makeGain: \a -> makeGain a
  , makeHighpass: \a -> makeHighpass a
  , makeHighshelf: \a -> makeHighshelf a
  , makeInput: \a -> makeInput a
  , makeLoopBuf: \a -> makeLoopBuf a
  , makeLoopBufWithDeferredBuffer: \a -> makeLoopBufWithDeferredBuffer a
  , makeLowpass: \a -> makeLowpass a
  , makeLowshelf: \a -> makeLowshelf a
  , makeMediaElement: \a -> makeMediaElement a
  , makeMicrophone: \a -> makeMicrophone a
  , makeNotch: \a -> makeNotch a
  , makePeaking: \a -> makePeaking a
  , makePeriodicOscWithDeferredOsc: \a -> makePeriodicOscWithDeferredOsc a
  , makePeriodicOsc: \{ id, spec, onOff, freq } -> (unwrap spec) # match
      { wave: \wave -> makePeriodicOsc { id, wave, onOff, freq }
      , realImg: \realImg -> makePeriodicOscV { id, realImg, onOff, freq }
      }
  , makePlayBuf: \a -> makePlayBuf a
  , makePlayBufWithDeferredBuffer: \a -> makePlayBufWithDeferredBuffer a
  , makeRecorder: \a -> makeRecorder a
  , makeSawtoothOsc: \a -> makeSawtoothOsc a
  , makeSinOsc: \a -> makeSinOsc a
  , makeSquareOsc: \a -> makeSquareOsc a
  , makeSpeaker: const $ makeSpeaker
  , makeStereoPanner: \a -> makeStereoPanner a
  , makeTriangleOsc: \a -> makeTriangleOsc a
  , makeWaveShaper: \a -> makeWaveShaper a
  -----------------------------------
  -- for now, we cannot get back tumult and subgraph
  -- add dummies
  -- maybe figure this out in the future
  , makeSubgraph: \{ id } -> makeGain { id, gain: paramize 1.0 }
  , makeTumult: \{ id } -> makeGain { id, gain: paramize 1.0 }
  -----------------------------------
  , connectXToY: \a -> connectXToY a
  , setAnalyserNodeCb: \a -> setAnalyserNodeCb a
  , setMediaRecorderCb: \a -> setMediaRecorderCb a
  , setAudioWorkletParameter: \a -> setAudioWorkletParameter a
  , setBuffer: \a -> setBuffer a
  , setConvolverBuffer: \a -> setConvolverBuffer a
  , setPeriodicOsc: \{ id, periodicOsc } -> (unwrap periodicOsc) # match
      { wave: \wave -> setPeriodicOsc { id, wave }
      , realImg: \realImg -> setPeriodicOscV { id, realImg }
      }
  , setOnOff: \a -> setOnOff a
  , setBufferOffset: \a -> setBufferOffset a
  , setDuration: \a -> setDuration a
  , setLoopStart: \a -> setLoopStart a
  , setLoopEnd: \a -> setLoopEnd a
  , setRatio: \a -> setRatio a
  , setOffset: \a -> setOffset a
  , setAttack: \a -> setAttack a
  , setGain: \a -> setGain a
  , setQ: \a -> setQ a
  , setPan: \a -> setPan a
  , setThreshold: \a -> setThreshold a
  , setRelease: \a -> setRelease a
  , setKnee: \a -> setKnee a
  , setDelay: \a -> setDelay a
  , setPlaybackRate: \a -> setPlaybackRate a
  , setFrequency: \a -> setFrequency a
  , setWaveShaperCurve: \a -> setWaveShaperCurve a
  , setInput: \a -> setInput a
  , setSubgraph: \{ id } -> setGain { id, gain: paramize 1.0 }
  , setTumult: \{ id } -> setGain { id, gain: paramize 1.0 }
  }

makeInstructionsEffectful
  :: Array Instruction
  -> Maybe (Array Instruction)
  -> Array (FFIAudioSnapshot -> Effect Unit)
makeInstructionsEffectful a = case _ of
  Nothing -> map (interpretInstruction) a
  Just b -> map (interpretInstruction)
    ( Array.fromFoldable $ reconcileTumult (Set.fromFoldable a)
        (Set.fromFoldable b)
    )

envsToFFI
  :: forall index env
   . Array (Ie index env)
  -> Array (Pie index env)
envsToFFI = map go
  where
  go { pos, index, env } =
    { pos, index, env: toNullable $ VM.maybe Nothing Just env }

instance effectfulAudioInterpret ::
  AudioInterpret FFIAudioSnapshot (Effect Unit) where
  connectXToY = connectXToY_
  disconnectXFromY = disconnectXFromY_
  destroyUnit = destroyUnit_
  makeInput = makeInput_
  makeAllpass = makeAllpass_
  makeAnalyser = makeAnalyser_
  makeAudioWorkletNode = makeAudioWorkletNode_
  makeBandpass = makeBandpass_
  makeConstant = makeConstant_
  makeConvolver = makeConvolver_
  makePassthroughConvolver = makePassthroughConvolver_
  makeDelay = makeDelay_
  makeDynamicsCompressor = makeDynamicsCompressor_
  makeGain = makeGain_
  makeHighpass = makeHighpass_
  makeHighshelf = makeHighshelf_
  makeLoopBufWithDeferredBuffer = makeLoopBufWithDeferredBuffer_
  makeLoopBuf = makeLoopBuf_
  makeLowpass = makeLowpass_
  makeLowshelf = makeLowshelf_
  makeMediaElement = makeMediaElement_
  makeMicrophone = makeMicrophone_
  makeNotch = makeNotch_
  makePeaking = makePeaking_
  makePeriodicOscWithDeferredOsc = makePeriodicOscWithDeferredOsc_
  makePeriodicOsc = makePeriodicOsc_
  makePeriodicOscV = makePeriodicOscV_
  makePlayBufWithDeferredBuffer = makePlayBufWithDeferredBuffer_
  makePlayBuf = makePlayBuf_
  makeRecorder = makeRecorder_
  makeSawtoothOsc = makeSawtoothOsc_
  makeSinOsc = makeSinOsc_
  makeSpeaker = makeSpeaker_
  makeSquareOsc = makeSquareOsc_
  makeStereoPanner = makeStereoPanner_
  {-: forall index env scene
   . String
  -> String
  -> Array (Pie index env)
  -> (index -> scene)
  -> ( env
       -> scene
       -> { instructions :: Array (FFIAudioSnapshot -> Effect Unit)
          , nextScene :: scene
          }
     )
  -> FFIAudioSnapshot
  -> Effect Unit-}
  makeSubgraph { id, terminus, envs, scenes } audio =
    makeSubgraph_ id
      (reflectSymbol terminus)
      (envsToFFI envs)
      scenes
      ( \env scene ->
          let
            res = oneSubFrame scene env
          in
            { instructions: res.instructions, nextScene: res.next }
      )
      audio
  makeTumult { id, terminus, instructions } toFFI =
    makeTumult_
      id
      terminus
      instructions
      Nothing
      Just
      makeInstructionsEffectful
      toFFI
  makeTriangleOsc = makeTriangleOsc_
  makeWaveShaper = makeWaveShaper_
  setAudioWorkletParameter = setAudioWorkletParameter_
  setAnalyserNodeCb = setAnalyserNodeCb_
  setMediaRecorderCb = setMediaRecorderCb_
  setConvolverBuffer = setConvolverBuffer_
  setBuffer = setBuffer_
  setPeriodicOsc = setPeriodicOsc_
  setPeriodicOscV = setPeriodicOscV_
  setOnOff = setOnOff_
  setBufferOffset = setBufferOffset_
  setDuration = setDuration_
  setLoopStart = setLoopStart_
  setLoopEnd = setLoopEnd_
  setRatio = setRatio_
  setOffset = setOffset_
  setAttack = setAttack_
  setGain = setGain_
  setQ = setQ_
  setPan = setPan_
  setThreshold = setThreshold_
  setRelease = setRelease_
  setKnee = setKnee_
  setDelay = setDelay_
  setPlaybackRate = setPlaybackRate_
  setFrequency = setFrequency_
  setWaveShaperCurve = setWaveShaperCurve_
  setInput = setInput_
  setSubgraph { id, envs } audio = setSubgraph_ id (envsToFFI envs)
    (audio)
  setTumult { id, terminus, instructions } toFFI = setTumult_
    id
    terminus
    instructions
    Nothing
    Just
    makeInstructionsEffectful
    (toFFI)

-- A utility typeclass used to convert PS arguments to arguments that are understood by the Web Audio API.

audioEngine1st
  :: forall terminus inputs env proof res
   . SubScene terminus inputs env (Unit /\ FFIAudioSnapshot)
       (Instruction /\ Effect Unit)
       proof
       res
  -> SubScene terminus inputs env Unit Instruction proof res
audioEngine1st (SubScene sceneA) = SubScene
  ( \env ->
      let
        eaA = sceneA env
      in
        -- highly unsafe. only works because the result is effectful and we don't have any form of currying in the
        -- ffi, so the effect won't be triggered and the `undefined` won't be used
        { instructions: map
            (\f aSide -> fst $ f (aSide /\ (unsafeCoerce undefined)))
            eaA.instructions
        , res: eaA.res
        , next: audioEngine1st eaA.next
        }
  )

audioEngine2nd
  :: forall terminus inputs env proof res
   . SubScene terminus inputs env (Unit /\ FFIAudioSnapshot)
       (Instruction /\ Effect Unit)
       proof
       res
  -> SubScene terminus inputs env FFIAudioSnapshot (Effect Unit) proof res
audioEngine2nd (SubScene sceneA) = SubScene
  ( \env ->
      let
        eaA = sceneA env
      in
        { instructions: map (\f bSide -> snd $ f (unit /\ bSide))
            eaA.instructions
        , res: eaA.res
        , next: audioEngine2nd eaA.next
        }
  )

instance mixedAudioInterpret ::
  AudioInterpret (Unit /\ FFIAudioSnapshot) (Instruction /\ Effect Unit) where
  connectXToY a (x /\ y) = connectXToY a x /\ connectXToY a y
  disconnectXFromY a (x /\ y) = disconnectXFromY a x /\ disconnectXFromY a y
  destroyUnit a (x /\ y) = destroyUnit a x /\ destroyUnit a y
  makeSubgraph { id, terminus, envs, scenes } (x /\ y) =
    makeSubgraph { id, terminus, envs, scenes: map audioEngine1st scenes } x /\
      makeSubgraph { id, terminus, envs, scenes: map audioEngine2nd scenes } y
  makeInput a (x /\ y) = makeInput a x /\ makeInput a y
  makeAllpass a (x /\ y) = makeAllpass a x /\ makeAllpass a y
  makeAnalyser a (x /\ y) = makeAnalyser a x /\ makeAnalyser a y
  makeAudioWorkletNode a (x /\ y) = makeAudioWorkletNode a x /\
    makeAudioWorkletNode a y
  makeBandpass a (x /\ y) = makeBandpass a x /\ makeBandpass a y
  makeConstant a (x /\ y) = makeConstant a x /\ makeConstant a y
  makePassthroughConvolver a (x /\ y) = makePassthroughConvolver a x /\
    makePassthroughConvolver a y
  makeConvolver a (x /\ y) = makeConvolver a x /\ makeConvolver a y
  makeDelay a (x /\ y) = makeDelay a x /\ makeDelay a y
  makeDynamicsCompressor a (x /\ y) = makeDynamicsCompressor a x /\
    makeDynamicsCompressor a y
  makeGain a (x /\ y) = makeGain a x /\ makeGain a y
  makeHighpass a (x /\ y) = makeHighpass a x /\ makeHighpass a y
  makeHighshelf a (x /\ y) = makeHighshelf a x /\ makeHighshelf a y
  makeLoopBufWithDeferredBuffer a (x /\ y) = makeLoopBufWithDeferredBuffer a x
    /\ makeLoopBufWithDeferredBuffer a y
  makeLoopBuf a (x /\ y) = makeLoopBuf a x /\ makeLoopBuf a y
  makeLowpass a (x /\ y) = makeLowpass a x /\ makeLowpass a y
  makeLowshelf a (x /\ y) = makeLowshelf a x /\ makeLowshelf a y
  makeMediaElement a (x /\ y) = makeMediaElement a x /\ makeMediaElement a y
  makeMicrophone a (x /\ y) = makeMicrophone a x /\ makeMicrophone a y
  makeNotch a (x /\ y) = makeNotch a x /\ makeNotch a y
  makePeaking a (x /\ y) = makePeaking a x /\ makePeaking a y
  makePeriodicOsc a (x /\ y) = makePeriodicOsc a x /\ makePeriodicOsc a y
  makePeriodicOscV a (x /\ y) = makePeriodicOscV a x /\ makePeriodicOscV a y
  makePeriodicOscWithDeferredOsc a (x /\ y) = makePeriodicOscWithDeferredOsc a x
    /\ makePeriodicOscWithDeferredOsc a y
  makePlayBufWithDeferredBuffer a (x /\ y) = makePlayBufWithDeferredBuffer a x
    /\ makePlayBufWithDeferredBuffer a y
  makePlayBuf a (x /\ y) = makePlayBuf a x /\ makePlayBuf a y
  makeRecorder a (x /\ y) = makeRecorder a x /\ makeRecorder a y
  makeSawtoothOsc a (x /\ y) = makeSawtoothOsc a x /\ makeSawtoothOsc a y
  makeSinOsc a (x /\ y) = makeSinOsc a x /\ makeSinOsc a y
  makeSpeaker (x /\ y) = makeSpeaker x /\ makeSpeaker y
  makeSquareOsc a (x /\ y) = makeSquareOsc a x /\ makeSquareOsc a y
  makeStereoPanner a (x /\ y) = makeStereoPanner a x /\ makeStereoPanner a y
  makeTriangleOsc a (x /\ y) = makeTriangleOsc a x /\ makeTriangleOsc a y
  makeWaveShaper a (x /\ y) = makeWaveShaper a x /\ makeWaveShaper a y
  makeTumult a (x /\ y) = makeTumult a x /\ makeTumult a y
  setAudioWorkletParameter a (x /\ y) = setAudioWorkletParameter a x /\
    setAudioWorkletParameter a y
  setAnalyserNodeCb a (x /\ y) = setAnalyserNodeCb a x /\ setAnalyserNodeCb a y
  setMediaRecorderCb a (x /\ y) = setMediaRecorderCb a x /\ setMediaRecorderCb a
    y
  setBuffer a (x /\ y) = setBuffer a x /\ setBuffer a y
  setConvolverBuffer a (x /\ y) = setConvolverBuffer a x /\ setConvolverBuffer a
    y
  setPeriodicOsc a (x /\ y) = setPeriodicOsc a x /\ setPeriodicOsc a y
  setPeriodicOscV a (x /\ y) = setPeriodicOscV a x /\ setPeriodicOscV a y
  setOnOff a (x /\ y) = setOnOff a x /\ setOnOff a y
  setBufferOffset a (x /\ y) = setBufferOffset a x /\ setBufferOffset a y
  setDuration a (x /\ y) = setDuration a x /\ setDuration a y
  setLoopStart a (x /\ y) = setLoopStart a x /\ setLoopStart a y
  setLoopEnd a (x /\ y) = setLoopEnd a x /\ setLoopEnd a y
  setRatio a (x /\ y) = setRatio a x /\ setRatio a y
  setOffset a (x /\ y) = setOffset a x /\ setOffset a y
  setAttack a (x /\ y) = setAttack a x /\ setAttack a y
  setGain a (x /\ y) = setGain a x /\ setGain a y
  setQ a (x /\ y) = setQ a x /\ setQ a y
  setPan a (x /\ y) = setPan a x /\ setPan a y
  setThreshold a (x /\ y) = setThreshold a x /\ setThreshold a y
  setRelease a (x /\ y) = setRelease a x /\ setRelease a y
  setKnee a (x /\ y) = setKnee a x /\ setKnee a y
  setDelay a (x /\ y) = setDelay a x /\ setDelay a y
  setPlaybackRate a (x /\ y) = setPlaybackRate a x /\ setPlaybackRate a y
  setFrequency a (x /\ y) = setFrequency a x /\ setFrequency a y
  setWaveShaperCurve a (x /\ y) = setWaveShaperCurve a x /\ setWaveShaperCurve a
    y
  setInput a (x /\ y) = setInput a x /\ setInput a y
  setSubgraph { id, envs } (x /\ y) = setSubgraph { id, envs } x /\ setSubgraph
    { id, envs }
    y
  setTumult a (x /\ y) = setTumult a x /\ setTumult a y
