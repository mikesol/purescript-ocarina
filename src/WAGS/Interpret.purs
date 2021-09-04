-- | Interpret audio in an engine using the `AudioInterpret` class.
-- |
-- | WAGS ships with two engines - `Effect Unit` for real audio and `Instruction` for an ADT representation of audio.
module WAGS.Interpret
  ( class AudioInterpret
  , class SafeToFFI
  , AudioBuffer
  , FFIAudioSnapshot'
  , FFIAudioSnapshot(..)
  , FFINumericAudioParameter
  , audioBuffer
  , audioWorkletAddModule
  , close
  , connectXToY
  , context
  , decodeAudioDataFromBase64EncodedString
  , decodeAudioDataFromUri
  , defaultFFIAudio
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
  , makeUnitCache
  , makeWaveShaper
  , mediaRecorderToUrl
  , renderAudio
  , safeToFFI
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
  ) where

import Prelude

import Control.Plus (empty)
import Control.Promise (Promise, toAffE)
import Data.ArrayBuffer.Types (Float32Array, Uint8Array)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe, isNothing, maybe)
import Data.Newtype (wrap)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Nat, class Pos)
import Data.Vec (Vec)
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Safe.Coerce (coerce)
import Simple.JSON as JSON
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Graph.AudioUnit (APOnOff, OnOff(..))
import WAGS.Graph.Parameter (AudioParameter_(..), AudioParameter)
import WAGS.Graph.Worklet (AudioWorkletNodeRequest, AudioWorkletNodeResponse)
import WAGS.Rendered (Instruction(..), Oversample(..))
import WAGS.Util (class ValidateOutputChannelCount, tmap)
import WAGS.WebAPI (AnalyserNode, AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorder, MediaRecorderCb)
import WAGS.WebAPI as WebAPI

foreign import getFFTSize :: WebAPI.AnalyserNode -> Effect Int

foreign import setFFTSize :: WebAPI.AnalyserNode -> Int -> Effect Unit

foreign import getFrequencyBinCount :: WebAPI.AnalyserNode -> Effect Int

foreign import getSmoothingTimeConstant :: WebAPI.AnalyserNode -> Effect Number

foreign import setSmoothingTimeConstant :: WebAPI.AnalyserNode -> Number -> Effect Unit

foreign import getMinDecibels :: WebAPI.AnalyserNode -> Effect Number

foreign import setMinDecibels :: WebAPI.AnalyserNode -> Number -> Effect Unit

foreign import getMaxDecibels :: WebAPI.AnalyserNode -> Effect Number

foreign import setMaxDecibels :: WebAPI.AnalyserNode -> Number -> Effect Unit

foreign import getFloatTimeDomainData :: WebAPI.AnalyserNode -> Effect Float32Array

foreign import getFloatFrequencyData :: WebAPI.AnalyserNode -> Effect Float32Array

foreign import getByteTimeDomainData :: WebAPI.AnalyserNode -> Effect Uint8Array

foreign import getByteFrequencyData :: WebAPI.AnalyserNode -> Effect Uint8Array

-- | For a given audio context, add the audio worklet module at a given URI.
foreign import audioWorkletAddModule_ :: WebAPI.AudioContext -> String -> Effect (Promise Unit)

-- | Gets the audio clock time from an audio context.
foreign import getAudioClockTime :: WebAPI.AudioContext -> Effect Number

-- | Stops a media recorder
foreign import stopMediaRecorder :: WebAPI.MediaRecorder -> Effect Unit

-- | For a given MIME type, pass the URL-ified content of a media recorder as a string to a handler.
-- |
-- | ```purescript
-- | mediaRecorderToUrl "audio/ogg" setAudioTagUrlToThisContent recorder
-- | ```
foreign import mediaRecorderToUrl :: String -> (String -> Effect Unit) -> WebAPI.MediaRecorder -> Effect Unit

-- | Is this MIME type supported by this browser.
foreign import isTypeSupported :: String -> Effect Boolean

-- | Given an audio context and a URI, decode the content of the URI to an audio buffer.
foreign import decodeAudioDataFromUri :: WebAPI.AudioContext -> String -> Effect (Promise WebAPI.BrowserAudioBuffer)

-- | Given an audio context and a base-64-encoded audio file, decode the content of the string to an audio buffer.
foreign import decodeAudioDataFromBase64EncodedString :: WebAPI.AudioContext -> String -> Effect (Promise WebAPI.BrowserAudioBuffer)

foreign import makePeriodicWaveImpl :: WebAPI.AudioContext -> Array Number -> Array Number -> Effect WebAPI.BrowserPeriodicWave

-- | For a given audio context, use an audio buffer to create a browser audio buffer. This is useful when doing DSP in the browser.  Note that `AudioBuffer` is a purescript type whereas `WebAPI.BrowserAudioBuffer` is an optimized browser-based type. That means that, once you write to `WebAPI.BrowserAudioBuffer`, it is effectively a blob and its contents cannot be retrieved using the WAGS API.
foreign import makeAudioBuffer :: WebAPI.AudioContext -> AudioBuffer -> Effect WebAPI.BrowserAudioBuffer

-- | Make a float 32 array. Useful when creating a waveshaper node.
foreign import makeFloatArray :: Array Number -> Effect WebAPI.BrowserFloatArray

-- | Make a new audio context.
foreign import context :: Effect WebAPI.AudioContext

-- | Close an audio context.
foreign import close :: WebAPI.AudioContext -> Effect Unit

foreign import data BrowserMediaStream :: Type

foreign import getBrowserMediaStreamImpl :: Boolean -> Boolean -> Effect (Promise BrowserMediaStream)

data Audio

audioWorkletAddModule
  :: forall node numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions
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
audioWorkletAddModule c px _ = toAffE (audioWorkletAddModule_ c px) $> unsafeCoerce unit

browserMediaStreamToBrowserMicrophone :: BrowserMediaStream -> WebAPI.BrowserMicrophone
browserMediaStreamToBrowserMicrophone = unsafeCoerce

browserMediaStreamToBrowserCamera :: BrowserMediaStream -> WebAPI.BrowserCamera
browserMediaStreamToBrowserCamera = unsafeCoerce

getMicrophoneAndCamera :: Boolean -> Boolean -> Aff { microphone :: Maybe WebAPI.BrowserMicrophone, camera :: Maybe WebAPI.BrowserCamera }
getMicrophoneAndCamera audio video =
  ( \i ->
      { microphone: if audio then pure $ browserMediaStreamToBrowserMicrophone i else empty
      , camera: if video then pure $ browserMediaStreamToBrowserCamera i else empty
      }
  )
    <$> toAffE (getBrowserMediaStreamImpl audio video)

-- | Create a unit cache. This returns a fresh empty object `{}` that is used to cache audio units.
foreign import makeUnitCache :: Effect Foreign

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
   . Pos len
  => WebAPI.AudioContext
  -> Vec len Number
  -> Vec len Number
  -> Effect WebAPI.BrowserPeriodicWave
makePeriodicWave ctx a b = makePeriodicWaveImpl ctx (V.toArray a) (V.toArray b)

-- | A multi-channel audio buffer.
data AudioBuffer
  = AudioBuffer Int (Array (Array Number))

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

-- | The audio information that goes to the ffi during rendering.
-- |
-- | - `context` - the audio context. To create an `WebAPI.AudioContext`, use `context`.
-- | - `writeHead` - the moment in time in the audio context at which we are writing. The easing algorithm provided to run makes sure that this is always slightly ahead of the actual audio context time. When starting a scene, this should be set to `0.0`.
-- | - `units` - an object in which audio units are cached and retrieved. To create this, use `makeUnitCache`.
-- | - `microphone` - the browser microphone or `null` if we do not have one.
-- | - `recorders` - an object recorder rendering functions. Because media recorders do not yet exist when a scene starts, we provide rendering functions and then fill in the actual recorders once the rendering starts.
-- | - `buffers` - an object containing named audio buffers for playback using `PlayBuf` or `LoopBuf`. See the `atari-speaks` example to see how a buffer is used.
-- | - `floatArrays` - arrays of 32=bit floats used for wave shaping.
-- | - `periodicWaves` - array of periodic waves used for creating oscillator nodes.
type FFIAudioSnapshot'
  =
  { context :: WebAPI.AudioContext
  , writeHead :: Number
  , units :: Foreign
  }

type DefaultFFIAudioWithBehaviors
  =
  { context :: WebAPI.AudioContext
  , writeHead :: Number
  , units :: Foreign
  }

-- A default FFI audio with empty objects (ie no buffers, no microphone, etc).
defaultFFIAudio :: WebAPI.AudioContext -> Foreign -> DefaultFFIAudioWithBehaviors
defaultFFIAudio audioCtx unitCache =
  { context: audioCtx
  , writeHead: 0.0
  , units: unitCache
  }

-- FFIAudio as a newtype in order to use it in typeclass instances.
newtype FFIAudioSnapshot
  = FFIAudioSnapshot FFIAudioSnapshot'

-- | A class with all possible instructions for interpreting audio.
-- | The class is paramaterized by two types:
-- | - `audio`: an audio context, which could be nothing (ie `Unit`) if there is audio or `FFIAudio` if there is audio.
-- | - `engine`: the output of the engine. For real audio, this is `Effect Unit`, as playing something from a loudspeaker is a side effect that doesn't return anything. For testing, this is the `Instruction` type, which is an ADT representation of instructions to an audio engine.
class AudioInterpret audio engine where
  -- | Connect pointer x to pointer y. For example, connect a sine wave oscillator to a highpass filter.
  connectXToY :: String -> String -> audio -> engine
  -- | Disconnect pointer x from pointer y. For example, disconnect a sine wave oscillator from a gain unit.
  disconnectXFromY :: String -> String -> audio -> engine
  -- | Destroy pointer x. For example, drop a sine wave oscillator from an audio graph. Note that this does not invoke garbage collection - it just removes the reference to the node, allowing it to be garbage collected.
  destroyUnit :: String -> audio -> engine
  -- | Make an allpass filter.
  makeAllpass :: String -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make an analyser.
  makeAnalyser :: String -> AnalyserNodeCb -> audio -> engine
  -- | Make an audio worklet node.
  makeAudioWorkletNode :: String -> String -> Foreign -> audio -> engine
  -- | Make a bandpass filter.
  makeBandpass :: String -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a constant source, ie a stream of 0s.
  makeConstant :: String -> APOnOff -> AudioParameter -> audio -> engine
  -- | Make a "passthrough" convolver where the buffer will be set later.
  makePassthroughConvolver :: String -> audio -> engine
  -- | Make a convolution unit, aka reverb.
  makeConvolver :: String -> BrowserAudioBuffer -> audio -> engine
  -- | Make a delay unit.
  makeDelay :: String -> AudioParameter -> audio -> engine
  -- | Make a compressor/expander.
  makeDynamicsCompressor :: String -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a gain unit.
  makeGain :: String -> AudioParameter -> audio -> engine
  -- | Make a highpass filter.
  makeHighpass :: String -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a highshelf filter.
  makeHighshelf :: String -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a looping audio buffer node with a deferred buffer.
  makeLoopBufWithDeferredBuffer :: String -> audio -> engine
  -- | Make a looping audio buffer node.
  makeLoopBuf :: String -> BrowserAudioBuffer -> APOnOff -> AudioParameter -> Number -> Number -> audio -> engine
  -- | Make a lowpass filter.
  makeLowpass :: String -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a lowshelf filter.
  makeLowshelf :: String -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a microphone.
  makeMicrophone :: BrowserMicrophone -> audio -> engine
  -- | Make a notch filter.
  makeNotch :: String -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a peaking filter.
  makePeaking :: String -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a periodic oscillator.
  makePeriodicOscWithDeferredOsc :: String -> audio -> engine
  -- | Make a periodic oscillator.
  makePeriodicOsc :: String -> BrowserPeriodicWave -> APOnOff -> AudioParameter -> audio -> engine
  -- | Make a periodic oscillator
  makePeriodicOscV :: forall (a :: Type). String -> V.Vec a Number /\ V.Vec a Number -> APOnOff -> AudioParameter -> audio -> engine
  -- | Make an audio buffer node with a deferred buffer.
  makePlayBufWithDeferredBuffer :: String -> audio -> engine
  -- | Make an audio buffer node.
  makePlayBuf :: String -> BrowserAudioBuffer -> Number -> APOnOff -> AudioParameter -> audio -> engine
  -- | Make a recorder.
  makeRecorder :: String -> MediaRecorderCb -> audio -> engine
  -- | Make a sawtooth oscillator.
  makeSawtoothOsc :: String -> APOnOff -> AudioParameter -> audio -> engine
  -- | Make a sine-wave oscillator.
  makeSinOsc :: String -> APOnOff -> AudioParameter -> audio -> engine
  -- | Make a node representing the loudspeaker. For sound to be rendered, it must go to a loudspeaker.
  makeSpeaker :: audio -> engine
  -- | Make a square-wave oscillator.
  makeSquareOsc :: String -> APOnOff -> AudioParameter -> audio -> engine
  -- | Make a stereo panner
  makeStereoPanner :: String -> AudioParameter -> audio -> engine
  -- | Make a triangle-wave oscillator.
  makeTriangleOsc :: String -> APOnOff -> AudioParameter -> audio -> engine
  -- | Make a wave shaper.
  makeWaveShaper :: String -> BrowserFloatArray -> Oversample -> audio -> engine
  -- | Sets the callback used by an analyser node
  setAnalyserNodeCb :: String -> AnalyserNodeCb -> audio -> engine
  -- | Sets the callback used by a recorder node
  setMediaRecorderCb :: String -> MediaRecorderCb -> audio -> engine
  -- | Sets the waveshaper curve float array
  setWaveShaperCurve :: String -> BrowserFloatArray -> audio -> engine
  -- | Sets a custom parameter for an audio worklet node
  setAudioWorkletParameter :: String -> String -> AudioParameter -> audio -> engine
  -- | Sets the buffer to read from in a playBuf or loopBuf
  setBuffer :: String -> BrowserAudioBuffer -> audio -> engine
  -- | Sets the buffer to read from in a convolver
  setConvolverBuffer :: String -> BrowserAudioBuffer -> audio -> engine
  -- | Sets the periodic oscillator to read from in a periodicOsc
  setPeriodicOsc :: String -> BrowserPeriodicWave -> audio -> engine
  -- | Sets the periodic oscillator to read from in a periodicOsc
  setPeriodicOscV :: forall (a :: Type). String -> V.Vec a Number /\ V.Vec a Number -> audio -> engine
  -- | Turn on or off a generator (an oscillator or playback node).
  setOnOff :: String -> APOnOff -> audio -> engine
  -- | Set the offset for a playbuf
  setBufferOffset :: String -> Number -> audio -> engine
  -- | Set the start position of a looping audio buffer node.
  setLoopStart :: String -> Number -> audio -> engine
  -- | Set the end position of a looping audio buffer node.
  setLoopEnd :: String -> Number -> audio -> engine
  -- | Set the ratio of a compressor.
  setRatio :: String -> AudioParameter -> audio -> engine
  -- | Set the offset of a constant source node.
  setOffset :: String -> AudioParameter -> audio -> engine
  -- | Set the attack of a compressor.
  setAttack :: String -> AudioParameter -> audio -> engine
  -- | Set the gain of a gain node or filter.
  setGain :: String -> AudioParameter -> audio -> engine
  -- | Set the q of a biquad filter.
  setQ :: String -> AudioParameter -> audio -> engine
  -- | Set the pan of a stereo panner.
  setPan :: String -> AudioParameter -> audio -> engine
  -- | Set the threshold of a compressor.
  setThreshold :: String -> AudioParameter -> audio -> engine
  -- | Set the release of a compressor.
  setRelease :: String -> AudioParameter -> audio -> engine
  -- | Set the knee of a compressor.
  setKnee :: String -> AudioParameter -> audio -> engine
  -- | Set the delay of a delay node.
  setDelay :: String -> AudioParameter -> audio -> engine
  -- | Set the playback rate of an audio node buffer or loop buffer.
  setPlaybackRate :: String -> AudioParameter -> audio -> engine
  -- | Set the frequency of an oscillator or filter.
  setFrequency :: String -> AudioParameter -> audio -> engine

instance freeAudioInterpret :: AudioInterpret Unit Instruction where
  connectXToY a b = const $ ConnectXToY a b
  disconnectXFromY a b = const $ DisconnectXFromY a b
  destroyUnit a = const $ DestroyUnit a
  makeAllpass a b c = const $ MakeAllpass a b c
  makeAnalyser a b = const $ MakeAnalyser a b
  makeAudioWorkletNode a b c = const $ MakeAudioWorkletNode a b (wrap c)
  makeBandpass a b c = const $ MakeBandpass a b c
  makeConstant a b c = const $ MakeConstant a b c
  makePassthroughConvolver a = const $ MakePassthroughConvolver a
  makeConvolver a b = const $ MakeConvolver a b
  makeDelay a b = const $ MakeDelay a b
  makeDynamicsCompressor a b c d e f = const $ MakeDynamicsCompressor a b c d e f
  makeGain a b = const $ MakeGain a b
  makeHighpass a b c = const $ MakeHighpass a b c
  makeHighshelf a b c = const $ MakeHighshelf a b c
  makeLoopBufWithDeferredBuffer a = const $ MakeLoopBufWithDeferredBuffer a
  makeLoopBuf a b c d e f = const $ MakeLoopBuf a b c d e f
  makeLowpass a b c = const $ MakeLowpass a b c
  makeLowshelf a b c = const $ MakeLowshelf a b c
  makeMicrophone a = const $ MakeMicrophone a
  makeNotch a b c = const $ MakeNotch a b c
  makePeaking a b c d = const $ MakePeaking a b c d
  makePeriodicOsc a b c d = const $ MakePeriodicOsc a (Left b) c d
  makePeriodicOscV a b c d = const $ MakePeriodicOsc a (Right (tmap V.toArray b)) c d
  makePeriodicOscWithDeferredOsc a = const $ MakePeriodicOscWithDeferredOsc a
  makePlayBufWithDeferredBuffer a = const $ MakePlayBufWithDeferredBuffer a
  makePlayBuf a b c d e = const $ MakePlayBuf a b c d e
  makeRecorder a b = const $ MakeRecorder a b
  makeSawtoothOsc a b c = const $ MakeSawtoothOsc a b c
  makeSinOsc a b c = const $ MakeSinOsc a b c
  makeSpeaker = const $ MakeSpeaker
  makeSquareOsc a b c = const $ MakeSquareOsc a b c
  makeStereoPanner a b = const $ MakeStereoPanner a b
  makeTriangleOsc a b c = const $ MakeTriangleOsc a b c
  makeWaveShaper a b c = const $ MakeWaveShaper a b c
  setAudioWorkletParameter a b c = const $ SetAudioWorkletParameter a b c
  setBuffer a b = const $ SetBuffer a b
  setConvolverBuffer a b = const $ SetConvolverBuffer a b
  setPeriodicOsc a b = const $ SetPeriodicOsc a (Left b)
  setPeriodicOscV a b = const $ SetPeriodicOsc a (Right (tmap V.toArray b))
  setOnOff a b = const $ SetOnOff a b
  setMediaRecorderCb a b = const $ SetMediaRecorderCb a b
  setAnalyserNodeCb a b = const $ SetAnalyserNodeCb a b
  setBufferOffset a b = const $ SetBufferOffset a b
  setLoopStart a b = const $ SetLoopStart a b
  setLoopEnd a b = const $ SetLoopEnd a b
  setRatio a b = const $ SetRatio a b
  setOffset a b = const $ SetOffset a b
  setAttack a b = const $ SetAttack a b
  setGain a b = const $ SetGain a b
  setQ a b = const $ SetQ a b
  setPan a b = const $ SetPan a b
  setThreshold a b = const $ SetThreshold a b
  setRelease a b = const $ SetRelease a b
  setKnee a b = const $ SetKnee a b
  setDelay a b = const $ SetDelay a b
  setPlaybackRate a b = const $ SetPlaybackRate a b
  setFrequency a b = const $ SetFrequency a b
  setWaveShaperCurve a b = const $ SetWaveShaperCurve a b

foreign import connectXToY_ :: String -> String -> FFIAudioSnapshot' -> Effect Unit

foreign import disconnectXFromY_ :: String -> String -> FFIAudioSnapshot' -> Effect Unit

foreign import destroyUnit_ :: String -> FFIAudioSnapshot' -> Effect Unit

foreign import makeAllpass_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeAnalyser_ :: String -> (AnalyserNode -> Effect (Effect Unit)) -> FFIAudioSnapshot' -> Effect Unit

foreign import makeAudioWorkletNode_ :: String -> String -> Foreign -> FFIAudioSnapshot' -> Effect Unit

foreign import makeBandpass_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeConstant_ :: String -> FFIStringAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeConvolver_ :: String -> BrowserAudioBuffer -> FFIAudioSnapshot' -> Effect Unit

foreign import makePassthroughConvolver_ :: String -> FFIAudioSnapshot' -> Effect Unit

foreign import makeDelay_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeDynamicsCompressor_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeGain_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeHighpass_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeHighshelf_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeLoopBufWithDeferredBuffer_ :: String -> FFIAudioSnapshot' -> Effect Unit

foreign import makeLoopBuf_ :: String -> BrowserAudioBuffer -> FFIStringAudioParameter -> FFINumericAudioParameter -> Number -> Number -> FFIAudioSnapshot' -> Effect Unit

foreign import makeLowpass_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeLowshelf_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeMicrophone_ :: BrowserMicrophone -> FFIAudioSnapshot' -> Effect Unit

foreign import makeNotch_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makePeaking_ :: String -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makePeriodicOscWithDeferredOsc_ :: String -> FFIAudioSnapshot' -> Effect Unit

foreign import makePeriodicOsc_ :: String -> BrowserPeriodicWave -> FFIStringAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makePeriodicOscV_ :: String -> (Array (Array Number)) -> FFIStringAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makePlayBufWithDeferredBuffer_ :: String -> FFIAudioSnapshot' -> Effect Unit

foreign import makePlayBuf_ :: String -> BrowserAudioBuffer -> Number -> FFIStringAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeRecorder_ :: String -> (MediaRecorder -> Effect Unit) -> FFIAudioSnapshot' -> Effect Unit

foreign import makeSawtoothOsc_ :: String -> FFIStringAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeSinOsc_ :: String -> FFIStringAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeSpeaker_ :: FFIAudioSnapshot' -> Effect Unit

foreign import makeSquareOsc_ :: String -> FFIStringAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeStereoPanner_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeTriangleOsc_ :: String -> FFIStringAudioParameter -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import makeWaveShaper_ :: String -> BrowserFloatArray -> String -> FFIAudioSnapshot' -> Effect Unit

foreign import setAudioWorkletParameter_ :: String -> String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setOnOff_ :: String -> FFIStringAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setBufferOffset_ :: String -> Number -> FFIAudioSnapshot' -> Effect Unit

foreign import setLoopStart_ :: String -> Number -> FFIAudioSnapshot' -> Effect Unit

foreign import setLoopEnd_ :: String -> Number -> FFIAudioSnapshot' -> Effect Unit

foreign import setRatio_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setOffset_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setAttack_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setGain_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setQ_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setPan_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setThreshold_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setRelease_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setKnee_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setDelay_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setPlaybackRate_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setFrequency_ :: String -> FFINumericAudioParameter -> FFIAudioSnapshot' -> Effect Unit

foreign import setBuffer_ :: String -> BrowserAudioBuffer -> FFIAudioSnapshot' -> Effect Unit

foreign import setConvolverBuffer_ :: String -> BrowserAudioBuffer -> FFIAudioSnapshot' -> Effect Unit

foreign import setPeriodicOsc_ :: String -> BrowserPeriodicWave -> FFIAudioSnapshot' -> Effect Unit

foreign import setPeriodicOscV_ :: String -> Array (Array Number) -> FFIAudioSnapshot' -> Effect Unit

foreign import setAnalyserNodeCb_ :: String -> (AnalyserNode -> Effect (Effect Unit)) -> FFIAudioSnapshot' -> Effect Unit

foreign import setMediaRecorderCb_ :: String -> (MediaRecorder ->  Effect Unit) -> FFIAudioSnapshot' -> Effect Unit

foreign import setWaveShaperCurve_ :: String -> BrowserFloatArray -> FFIAudioSnapshot' -> Effect Unit

instance effectfulAudioInterpret :: AudioInterpret FFIAudioSnapshot (Effect Unit) where
  connectXToY a b c = connectXToY_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  disconnectXFromY a b c = disconnectXFromY_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  destroyUnit a b = destroyUnit_ (safeToFFI a) (safeToFFI b)
  makeAllpass a b c d = makeAllpass_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeAnalyser a b c = makeAnalyser_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeAudioWorkletNode a b c d = makeAudioWorkletNode_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeBandpass a b c d = makeBandpass_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeConstant a b c d = makeConstant_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeConvolver a b c = makeConvolver_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makePassthroughConvolver a b = makePassthroughConvolver_ (safeToFFI a) (safeToFFI b)
  makeDelay a b c = makeDelay_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeDynamicsCompressor a b c d e f g = makeDynamicsCompressor_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e) (safeToFFI f) (safeToFFI g)
  makeGain a b c = makeGain_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeHighpass a b c d = makeHighpass_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeHighshelf a b c d = makeHighshelf_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeLoopBufWithDeferredBuffer a b = makeLoopBufWithDeferredBuffer_ (safeToFFI a) (safeToFFI b)
  makeLoopBuf a b c d e f g = makeLoopBuf_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e) (safeToFFI f) (safeToFFI g)
  makeLowpass a b c d = makeLowpass_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeLowshelf a b c d = makeLowshelf_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeMicrophone a b = makeMicrophone_ (safeToFFI a) (safeToFFI b)
  makeNotch a b c d = makeNotch_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makePeaking a b c d e = makePeaking_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e)
  makePeriodicOscWithDeferredOsc a b = makePeriodicOscWithDeferredOsc_ (safeToFFI a) (safeToFFI b)
  makePeriodicOsc a b c d e = makePeriodicOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e)
  makePeriodicOscV a b c d e = makePeriodicOscV_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e)
  makePlayBufWithDeferredBuffer a b = makePlayBufWithDeferredBuffer_ (safeToFFI a) (safeToFFI b)
  makePlayBuf a b c d e f = makePlayBuf_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e) (safeToFFI f)
  makeRecorder a b c = makeRecorder_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeSawtoothOsc a b c d = makeSawtoothOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeSinOsc a b c d = makeSinOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeSpeaker a = makeSpeaker_ (safeToFFI a)
  makeSquareOsc a b c d = makeSquareOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeStereoPanner a b c = makeStereoPanner_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeTriangleOsc a b c d = makeTriangleOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeWaveShaper a b c d = makeWaveShaper_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  setAudioWorkletParameter a b c d = setAudioWorkletParameter_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  setAnalyserNodeCb a b c = setAnalyserNodeCb_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setMediaRecorderCb a b c = setMediaRecorderCb_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setConvolverBuffer a b c = setConvolverBuffer_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setBuffer a b c = setBuffer_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setPeriodicOsc a b c = setPeriodicOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setPeriodicOscV a b c = setPeriodicOscV_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setOnOff a b c = setOnOff_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setBufferOffset a b c = setBufferOffset_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setLoopStart a b c = setLoopStart_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setLoopEnd a b c = setLoopEnd_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setRatio a b c = setRatio_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setOffset a b c = setOffset_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setAttack a b c = setAttack_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setGain a b c = setGain_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setQ a b c = setQ_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setPan a b c = setPan_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setThreshold a b c = setThreshold_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setRelease a b c = setRelease_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setKnee a b c = setKnee_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setDelay a b c = setDelay_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setPlaybackRate a b c = setPlaybackRate_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setFrequency a b c = setFrequency_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  setWaveShaperCurve a b c = setWaveShaperCurve_ (safeToFFI a) (safeToFFI b) (safeToFFI c)

-- A utility typeclass used to convert PS arguments to arguments that are understood by the Web Audio API.
class SafeToFFI a b | a -> b where
  safeToFFI :: a -> b

instance safeToFFI_BrowserMicrophone :: SafeToFFI BrowserMicrophone BrowserMicrophone where
  safeToFFI = identity

instance safeToFFI_BrowserAudioBuffer :: SafeToFFI BrowserAudioBuffer BrowserAudioBuffer where
  safeToFFI = identity

instance safeToFFI_BrowserPeriodicWave :: SafeToFFI BrowserPeriodicWave BrowserPeriodicWave where
  safeToFFI = identity

instance safeToFFI_BrowserFloatArray :: SafeToFFI BrowserFloatArray BrowserFloatArray where
  safeToFFI = identity

instance safeToFFI_AnalyserNodeCb :: SafeToFFI AnalyserNodeCb (AnalyserNode -> Effect (Effect Unit)) where
  safeToFFI = coerce

instance safeToFFI_MediaRecorderCb :: SafeToFFI MediaRecorderCb (MediaRecorder -> Effect Unit) where
  safeToFFI = coerce

instance safeToFFI_Int :: SafeToFFI Int Int where
  safeToFFI = identity

instance safeToFFI_Number :: SafeToFFI Number Number where
  safeToFFI = identity

instance safeToFFI_Foreign :: SafeToFFI Foreign Foreign where
  safeToFFI = identity

instance safeToFFI_VecNumber :: SafeToFFI (V.Vec a Number /\ V.Vec a Number) (Array (Array Number)) where
  safeToFFI (a /\ b) = [ V.toArray a, V.toArray b ]

instance safeToFFI_String :: SafeToFFI String String where
  safeToFFI = identity

instance safeToFFI_Oversample :: SafeToFFI Oversample String where
  safeToFFI = case _ of
    None -> "none"
    TwoX -> "2x"
    FourX -> "4x"

instance safeToFFI_FFIAudio :: SafeToFFI FFIAudioSnapshot FFIAudioSnapshot' where
  safeToFFI (FFIAudioSnapshot x) = x

-- | An AudioParameter with the `transition` field stringly-typed for easier rendering in the FFI and cancelation as a boolean
type FFINumericAudioParameter
  =
  { param :: Number
  , timeOffset :: Number
  , transition :: String
  , cancel :: Boolean
  }

instance safeToFFI_AudioParameter ::
  SafeToFFI (AudioParameter_ Number) FFINumericAudioParameter where
  safeToFFI (AudioParameter { param, timeOffset, transition }) =
    { param: fromMaybe 0.0 param
    , timeOffset
    , transition: show transition
    , cancel: isNothing param
    }

-- | An AudioParameter with the `transition` field stringly-typed for easier rendering in the FFI and cancelation as a boolean
type FFIStringAudioParameter
  =
  { param :: String
  , timeOffset :: Number
  , transition :: String
  , cancel :: Boolean
  }

instance safeToFFI_AudioParameterString ::
  SafeToFFI (AudioParameter_ OnOff) FFIStringAudioParameter where
  safeToFFI (AudioParameter { param, timeOffset, transition }) =
    { param:
        maybe "off"
          ( case _ of
              On -> "on"
              Off -> "off"
              OffOn -> "offOn"
          )
          param
    , timeOffset
    , transition: show transition
    , cancel: isNothing param
    }

instance mixedAudioInterpret :: (AudioInterpret a c, AudioInterpret b d) => AudioInterpret (a /\ b) (c /\ d) where
  connectXToY a b (x /\ y) = connectXToY a b x /\ connectXToY a b y
  disconnectXFromY a b (x /\ y) = disconnectXFromY a b x /\ disconnectXFromY a b y
  destroyUnit a (x /\ y) = destroyUnit a x /\ destroyUnit a y
  makeAllpass a b c (x /\ y) = makeAllpass a b c x /\ makeAllpass a b c y
  makeAnalyser a b (x /\ y) = makeAnalyser a b x /\ makeAnalyser a b y
  makeAudioWorkletNode a b c (x /\ y) = makeAudioWorkletNode a b c x /\ makeAudioWorkletNode a b c y
  makeBandpass a b c (x /\ y) = makeBandpass a b c x /\ makeBandpass a b c y
  makeConstant a b c (x /\ y) = makeConstant a b c x /\ makeConstant a b c y
  makePassthroughConvolver a (x /\ y) = makePassthroughConvolver a x /\ makePassthroughConvolver a y
  makeConvolver a b (x /\ y) = makeConvolver a b x /\ makeConvolver a b y
  makeDelay a b (x /\ y) = makeDelay a b x /\ makeDelay a b y
  makeDynamicsCompressor a b c d e f (x /\ y) = makeDynamicsCompressor a b c d e f x /\ makeDynamicsCompressor a b c d e f y
  makeGain a b (x /\ y) = makeGain a b x /\ makeGain a b y
  makeHighpass a b c (x /\ y) = makeHighpass a b c x /\ makeHighpass a b c y
  makeHighshelf a b c (x /\ y) = makeHighshelf a b c x /\ makeHighshelf a b c y
  makeLoopBufWithDeferredBuffer a (x /\ y) = makeLoopBufWithDeferredBuffer a x /\ makeLoopBufWithDeferredBuffer a y
  makeLoopBuf a b c d e f (x /\ y) = makeLoopBuf a b c d e f x /\ makeLoopBuf a b c d e f y
  makeLowpass a b c (x /\ y) = makeLowpass a b c x /\ makeLowpass a b c y
  makeLowshelf a b c (x /\ y) = makeLowshelf a b c x /\ makeLowshelf a b c y
  makeMicrophone a (x /\ y) = makeMicrophone a x /\ makeMicrophone a y
  makeNotch a b c (x /\ y) = makeNotch a b c x /\ makeNotch a b c y
  makePeaking a b c d (x /\ y) = makePeaking a b c d x /\ makePeaking a b c d y
  makePeriodicOsc a b c d (x /\ y) = makePeriodicOsc a b c d x /\ makePeriodicOsc a b c d y
  makePeriodicOscV a b c d (x /\ y) = makePeriodicOscV a b c d x /\ makePeriodicOscV a b c d y
  makePeriodicOscWithDeferredOsc a (x /\ y) = makePeriodicOscWithDeferredOsc a x /\ makePeriodicOscWithDeferredOsc a y
  makePlayBufWithDeferredBuffer a (x /\ y) = makePlayBufWithDeferredBuffer a x /\ makePlayBufWithDeferredBuffer a y
  makePlayBuf a b c d e (x /\ y) = makePlayBuf a b c d e x /\ makePlayBuf a b c d e y
  makeRecorder a b (x /\ y) = makeRecorder a b x /\ makeRecorder a b y
  makeSawtoothOsc a b c (x /\ y) = makeSawtoothOsc a b c x /\ makeSawtoothOsc a b c y
  makeSinOsc a b c (x /\ y) = makeSinOsc a b c x /\ makeSinOsc a b c y
  makeSpeaker (x /\ y) = makeSpeaker x /\ makeSpeaker y
  makeSquareOsc a b c (x /\ y) = makeSquareOsc a b c x /\ makeSquareOsc a b c y
  makeStereoPanner a b (x /\ y) = makeStereoPanner a b x /\ makeStereoPanner a b y
  makeTriangleOsc a b c (x /\ y) = makeTriangleOsc a b c x /\ makeTriangleOsc a b c y
  makeWaveShaper a b c (x /\ y) = makeWaveShaper a b c x /\ makeWaveShaper a b c y
  setAudioWorkletParameter a b c (x /\ y) = setAudioWorkletParameter a b c x /\ setAudioWorkletParameter a b c y
  setAnalyserNodeCb a b (x /\ y) = setAnalyserNodeCb a b x /\ setAnalyserNodeCb a b y
  setMediaRecorderCb a b (x /\ y) = setMediaRecorderCb a b x /\ setMediaRecorderCb a b y
  setBuffer a b (x /\ y) = setBuffer a b x /\ setBuffer a b y
  setConvolverBuffer a b (x /\ y) = setConvolverBuffer a b x /\ setConvolverBuffer a b y
  setPeriodicOsc a b (x /\ y) = setPeriodicOsc a b x /\ setPeriodicOsc a b y
  setPeriodicOscV a b (x /\ y) = setPeriodicOscV a b x /\ setPeriodicOscV a b y
  setOnOff a b (x /\ y) = setOnOff a b x /\ setOnOff a b y
  setBufferOffset a b (x /\ y) = setBufferOffset a b x /\ setBufferOffset a b y
  setLoopStart a b (x /\ y) = setLoopStart a b x /\ setLoopStart a b y
  setLoopEnd a b (x /\ y) = setLoopEnd a b x /\ setLoopEnd a b y
  setRatio a b (x /\ y) = setRatio a b x /\ setRatio a b y
  setOffset a b (x /\ y) = setOffset a b x /\ setOffset a b y
  setAttack a b (x /\ y) = setAttack a b x /\ setAttack a b y
  setGain a b (x /\ y) = setGain a b x /\ setGain a b y
  setQ a b (x /\ y) = setQ a b x /\ setQ a b y
  setPan a b (x /\ y) = setPan a b x /\ setPan a b y
  setThreshold a b (x /\ y) = setThreshold a b x /\ setThreshold a b y
  setRelease a b (x /\ y) = setRelease a b x /\ setRelease a b y
  setKnee a b (x /\ y) = setKnee a b x /\ setKnee a b y
  setDelay a b (x /\ y) = setDelay a b x /\ setDelay a b y
  setPlaybackRate a b (x /\ y) = setPlaybackRate a b x /\ setPlaybackRate a b y
  setFrequency a b (x /\ y) = setFrequency a b x /\ setFrequency a b y
  setWaveShaperCurve a b (x /\ y) = setWaveShaperCurve a b x /\ setWaveShaperCurve a b y
