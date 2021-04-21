-- | Interpret audio in an engine using the `AudioInterpret` class.
-- |
-- | WAGS ships with two engines - `Effect Unit` for real audio and `Instruction` for an ADT representation of audio.
module WAGS.Interpret
  ( class AudioInterpret
  , class SafeToFFI
  , AudioBuffer
  , AudioContext
  , BrowserAudioBuffer
  , BrowserCamera
  , BrowserFloatArray
  , BrowserMicrophone
  , BrowserPeriodicWave
  , FFIAudio(..)
  , FFIAudio'
  , FFIAudioParameter'
  , MediaRecorder
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
  , makeAllpass
  , makeAudioBuffer
  , makeBandpass
  , makeConstant
  , makeConvolver
  , makeDelay
  , makeDynamicsCompressor
  , makeFloatArray
  , makeGain
  , makeHighpass
  , makeHighshelf
  , makeLoopBuf
  , makeLowpass
  , makeLowshelf
  , makeMicrophone
  , makeNotch
  , makePeaking
  , makePeriodicOsc
  , makePeriodicWave
  , makePlayBuf
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
  , rebaseAllUnits
  , renderAudio
  , safeToFFI
  , setAttack
  , setDelay
  , setFrequency
  , setGain
  , setKnee
  , setLoopEnd
  , setLoopStart
  , setOff
  , setOffset
  , setOn
  , setPan
  , setPlaybackRate
  , setQ
  , setRatio
  , setRelease
  , setThreshold
  , stopMediaRecorder
  ) where

import Prelude

import Control.Plus (empty)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null)
import Data.Typelevel.Num (class Pos)
import Data.Vec (Vec)
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as O
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Graph.Constructors (OnOff(..))
import WAGS.Graph.Parameter (AudioParameter(..))
import WAGS.Rendered (Instruction(..), Oversample(..))

-- | A [MediaRecorder](https://developer.mozilla.org/en-US/docs/Web/API/MediaRecorder).
foreign import data MediaRecorder :: Type

-- | A [PeriodicWave](https://developer.mozilla.org/en-US/docs/Web/API/PeriodicWave).
foreign import data BrowserPeriodicWave :: Type

-- | An [AudioBuffer](https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer).
foreign import data BrowserAudioBuffer :: Type

-- | A [Float32Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array)
foreign import data BrowserFloatArray :: Type

-- | An [AudioContext](https://developer.mozilla.org/en-US/docs/Web/API/AudioContext)
foreign import data AudioContext :: Type

-- | The [MediaStream](https://developer.mozilla.org/en-US/docs/Web/API/MediaStream) object for a microphone.
foreign import data BrowserMicrophone :: Type

-- | The [MediaStream](https://developer.mozilla.org/en-US/docs/Web/API/MediaStream) object for a camera.
foreign import data BrowserCamera :: Type

-- | Gets the audio clock time from an audio context.
foreign import getAudioClockTime :: AudioContext -> Effect Number

-- | Stops a media recorder
foreign import stopMediaRecorder :: MediaRecorder -> Effect Unit

-- | For a given MIME type, pass the URL-ified content of a media recorder as a string to a handler.
-- |
-- | ```purescript
-- | mediaRecorderToUrl "audio/ogg" setAudioTagUrlToThisContent recorder
-- | ```
foreign import mediaRecorderToUrl :: String -> (String -> Effect Unit) -> MediaRecorder -> Effect Unit

-- | Is this MIME type supported by this browser.
foreign import isTypeSupported :: String -> Effect Boolean

-- | Given an audio context and a URI, decode the content of the URI to an audio buffer.
foreign import decodeAudioDataFromUri :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

-- | Given an audio context and a base-64-encoded audio file, decode the content of the string to an audio buffer.
foreign import decodeAudioDataFromBase64EncodedString :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

-- | For a given audio context, add the audio worklet module at a given URI.
foreign import audioWorkletAddModule :: AudioContext -> String -> Effect (Promise Unit)

foreign import makePeriodicWaveImpl :: AudioContext -> Array Number -> Array Number -> Effect BrowserPeriodicWave

-- | For a given audio context, use an audio buffer to create a browser audio buffer. This is useful when doing DSP in the browser.  Note that `AudioBuffer` is a purescript type whereas `BrowserAudioBuffer` is an optimized browser-based type. That means that, once you write to `BrowserAudioBuffer`, it is effectively a blob and its contents cannot be retrieved using the WAGS API.
foreign import makeAudioBuffer :: AudioContext -> AudioBuffer -> Effect BrowserAudioBuffer

-- | Make a float 32 array. Useful when creating a waveshaper node.
foreign import makeFloatArray :: Array Number -> Effect BrowserFloatArray

-- | Make a new audio context.
foreign import context :: Effect AudioContext

-- | Close an audio context.
foreign import close :: AudioContext -> Effect Unit

foreign import data BrowserMediaStream :: Type

foreign import getBrowserMediaStreamImpl :: Boolean -> Boolean -> Effect (Promise BrowserMediaStream)


browserMediaStreamToBrowserMicrophone :: BrowserMediaStream -> BrowserMicrophone
browserMediaStreamToBrowserMicrophone = unsafeCoerce

browserMediaStreamToBrowserCamera :: BrowserMediaStream -> BrowserCamera
browserMediaStreamToBrowserCamera = unsafeCoerce

getMicrophoneAndCamera :: Boolean -> Boolean -> Aff { microphone :: Maybe BrowserMicrophone, camera :: Maybe BrowserCamera }
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
foreign import renderAudio :: FFIAudio -> Array (FFIAudio -> Effect Unit) -> Effect Unit

-- | Make a browser periodic wave. A PureScript-ified version of the periodic wave constructor
-- | from the [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/PeriodicWave/PeriodicWave).
-- | Given an audio context, a vector of real parts of complex numbers, and a vector of imaginary parts of complex numbers, build a periodic wave interpretable by the Web Audio API.
makePeriodicWave ::
  forall len.
  Pos len =>
  AudioContext ->
  Vec len Number ->
  Vec len Number ->
  Effect BrowserPeriodicWave
makePeriodicWave ctx a b = makePeriodicWaveImpl ctx (V.toArray a) (V.toArray b)

-- | A multi-channel audio buffer.
data AudioBuffer
  = AudioBuffer Int (Array (Array Number))

instance showAudioBuffer :: Show AudioBuffer where
  show (AudioBuffer i a) = "AudioBuffer <" <> show i <> " : " <> show a <> ">"

derive instance eqAudioBuffer :: Eq AudioBuffer

-- | Make a multi-channel audio buffer. Each vector into the multi-channel buffer must be the same length.
audioBuffer ::
  forall bch blen.
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  AudioBuffer
audioBuffer i v = AudioBuffer i (map V.toArray $ V.toArray v)

-- | The audio information that goes to the ffi during rendering.
-- |
-- | - `context` - the audio context. To create an `AudioContext`, use `context`.
-- | - `writeHead` - the moment in time in the audio context at which we are writing. The easing algorithm provided to run makes sure that this is always slightly ahead of the actual audio context time. When starting a scene, this should be set to `0.0`.
-- | - `units` - an object in which audio units are cached and retrieved. To create this, use `makeUnitCache`.
-- | - `microphone` - the browser microphone or `null` if we do not have one.
-- | - `recorders` - an object recorder rendering functions. Because media recorders do not yet exist when a scene starts, we provide rendering functions and then fill in the actual recorders once the rendering starts.
-- | - `buffers` - an object containing named audio buffers for playback using `PlayBuf` or `LoopBuf`. See the `atari-speaks` example to see how a buffer is used.
-- | - `floatArrays` - arrays of 32=bit floats used for wave shaping.
-- | - `periodicWaves` - array of periodic waves used for creating oscillator nodes.
type FFIAudio'
  = { context :: AudioContext
    , writeHead :: Number
    , units :: Foreign
    , microphone :: Nullable BrowserMicrophone
    , recorders :: Object (MediaRecorder -> Effect Unit)
    , buffers :: Object BrowserAudioBuffer
    , floatArrays :: Object BrowserFloatArray
    , periodicWaves :: Object BrowserPeriodicWave
    }

-- A default FFI audio with empty objects (ie no buffers, no microphone, etc).
defaultFFIAudio :: AudioContext -> Foreign -> FFIAudio'
defaultFFIAudio audioCtx unitCache =
  { context: audioCtx
  , writeHead: 0.0
  , units: unitCache
  , microphone: null
  , recorders: O.empty
  , buffers: O.empty
  , floatArrays: O.empty
  , periodicWaves: O.empty
  }

-- FFIAudio as a newtype in order to use it in typeclass instances.
newtype FFIAudio
  = FFIAudio FFIAudio'

-- | A class with all possible instructions for interpreting audio.
-- | The class is paramaterized by two types:
-- | - `audio`: an audio context, which could be nothing (ie `Unit`) if there is audio or `FFIAudio` if there is audio.
-- | - `engine`: the output of the engine. For real audio, this is `Effect Unit`, as playing something from a loudspeaker is a side effect that doesn't return anything. For testing, this is the `Instruction` type, which is an ADT representation of instructions to an audio engine.
class AudioInterpret audio engine where
  -- | Connect pointer x to pointer y. For example, connect a sine wave oscillator to a highpass filter.
  connectXToY :: Int -> Int -> audio -> engine
  -- | Disconnect pointer x from pointer y. For example, disconnect a sine wave oscillator from a gain unit.
  disconnectXFromY :: Int -> Int -> audio -> engine
  -- | Destroy pointer x. For example, drop a sine wave oscillator from an audio graph. Note that this does not invoke garbage collection - it just removes the reference to the node, allowing it to be garbage collected.
  destroyUnit :: Int -> audio -> engine
  -- | Rebases the pointers to audio units.
  rebaseAllUnits :: Array { from :: Int, to :: Int } -> audio -> engine
  -- | Make an allpass filter.
  makeAllpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a bandpass filter.
  makeBandpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a constant source, ie a stream of 0s.
  makeConstant :: Int -> OnOff -> AudioParameter -> audio -> engine
  -- | Make a convolution unit, aka reverb.
  makeConvolver :: Int -> String -> audio -> engine
  -- | Make a delay unit.
  makeDelay :: Int -> AudioParameter -> audio -> engine
  -- | Make a compressor/expander.
  makeDynamicsCompressor :: Int -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a gain unit.
  makeGain :: Int -> AudioParameter -> audio -> engine
  -- | Make a highpass filter.
  makeHighpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a highshelf filter.
  makeHighshelf :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  -- | Make a looping audio buffer node.
  makeLoopBuf :: Int -> String -> OnOff -> AudioParameter -> Number -> Number -> audio -> engine
  -- | Make a lowpass filter.
  makeLowpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine
   -- | Make a lowshelf filter.
  makeLowshelf :: Int -> AudioParameter -> AudioParameter -> audio -> engine
   -- | Make a microphone.
  makeMicrophone :: Int -> audio -> engine
   -- | Make a notch filter.
  makeNotch :: Int -> AudioParameter -> AudioParameter -> audio -> engine
   -- | Make a peaking filter.
  makePeaking :: Int -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine
   -- | Make a periodic oscillator.
  makePeriodicOsc :: Int -> String -> OnOff -> AudioParameter -> audio -> engine
  -- | Make an audio buffer node.
  makePlayBuf :: Int -> String -> Number -> OnOff -> AudioParameter -> audio -> engine
  -- | Make a recorder.
  makeRecorder :: Int -> String -> audio -> engine
  -- | Make a sawtooth oscillator.
  makeSawtoothOsc :: Int -> OnOff -> AudioParameter -> audio -> engine
  -- | Make a sine-wave oscillator.
  makeSinOsc :: Int -> OnOff -> AudioParameter -> audio -> engine
  -- | Make a node representing the loudspeaker. For sound to be rendered, it must go to a loudspeaker.
  makeSpeaker :: Int -> audio -> engine
  -- | Make a square-wave oscillator.
  makeSquareOsc :: Int -> OnOff -> AudioParameter -> audio -> engine
  -- | Make a stereo panner
  makeStereoPanner :: Int -> AudioParameter -> audio -> engine
  -- | Make a triangle-wave oscillator.
  makeTriangleOsc :: Int -> OnOff -> AudioParameter -> audio -> engine
  -- | Make a wave shaper.
  makeWaveShaper :: Int -> String -> Oversample -> audio -> engine
  -- | Turn on a generator (an oscillator or playback node).
  setOn :: Int -> audio -> engine
  -- | Turn off a generator (an oscillator or playback node).
  setOff :: Int -> audio -> engine
  -- | Set the start position of a looping audio buffer node.
  setLoopStart :: Int -> Number -> audio -> engine
  -- | Set the end position of a looping audio buffer node.
  setLoopEnd :: Int -> Number -> audio -> engine
  -- | Set the ratio of a compressor.
  setRatio :: Int -> AudioParameter -> audio -> engine
  -- | Set the offset of a constant source node.
  setOffset :: Int -> AudioParameter -> audio -> engine
  -- | Set the attack of a compressor.
  setAttack :: Int -> AudioParameter -> audio -> engine
  -- | Set the gain of a gain node or filter.
  setGain :: Int -> AudioParameter -> audio -> engine
  -- | Set the q of a biquad filter.
  setQ :: Int -> AudioParameter -> audio -> engine
  -- | Set the pan of a stereo panner.
  setPan :: Int -> AudioParameter -> audio -> engine
  -- | Set the threshold of a compressor.
  setThreshold :: Int -> AudioParameter -> audio -> engine
  -- | Set the release of a compressor.
  setRelease :: Int -> AudioParameter -> audio -> engine
  -- | Set the knee of a compressor.
  setKnee :: Int -> AudioParameter -> audio -> engine
  -- | Set the delay of a delay node.
  setDelay :: Int -> AudioParameter -> audio -> engine
  -- | Set the playback rate of an audio node buffer or loop buffer.
  setPlaybackRate :: Int -> AudioParameter -> audio -> engine
  -- | Set the frequency of an oscillator or filter.
  setFrequency :: Int -> AudioParameter -> audio -> engine

instance freeAudioInterpret :: AudioInterpret Unit Instruction where
  connectXToY a b = const $ ConnectXToY a b
  disconnectXFromY a b = const $ DisconnectXFromY a b
  destroyUnit a = const $ DestroyUnit a
  rebaseAllUnits a = const $ RebaseAllUnits a
  makeAllpass a b c = const $ MakeAllpass a b c
  makeBandpass a b c = const $ MakeBandpass a b c
  makeConstant a b c = const $ MakeConstant a b c
  makeConvolver a b = const $ MakeConvolver a b
  makeDelay a b = const $ MakeDelay a b
  makeDynamicsCompressor a b c d e f = const $ MakeDynamicsCompressor a b c d e f
  makeGain a b = const $ MakeGain a b
  makeHighpass a b c = const $ MakeHighpass a b c
  makeHighshelf a b c = const $ MakeHighshelf a b c
  makeLoopBuf a b c d e f = const $ MakeLoopBuf a b c d e f
  makeLowpass a b c = const $ MakeLowpass a b c
  makeLowshelf a b c = const $ MakeLowshelf a b c
  makeMicrophone a = const $ MakeMicrophone a
  makeNotch a b c = const $ MakeNotch a b c
  makePeaking a b c d = const $ MakePeaking a b c d
  makePeriodicOsc a b c d = const $ MakePeriodicOsc a b c d
  makePlayBuf a b c d e = const $ MakePlayBuf a b c d e
  makeRecorder a b = const $ MakeRecorder a b
  makeSawtoothOsc a b c = const $ MakeSawtoothOsc a b c
  makeSinOsc a b c = const $ MakeSinOsc a b c
  makeSpeaker a = const $ MakeSpeaker a
  makeSquareOsc a b c = const $ MakeSquareOsc a b c
  makeStereoPanner a b = const $ MakeStereoPanner a b
  makeTriangleOsc a b c = const $ MakeTriangleOsc a b c
  makeWaveShaper a b c = const $ MakeWaveShaper a b c
  setOn a = const $ SetOn a
  setOff a = const $ SetOff a
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

foreign import connectXToY_ :: Int -> Int -> FFIAudio' -> Effect Unit

foreign import disconnectXFromY_ :: Int -> Int -> FFIAudio' -> Effect Unit

foreign import destroyUnit_ :: Int -> FFIAudio' -> Effect Unit

foreign import rebaseAllUnits_ :: Array { from :: Int, to :: Int } -> FFIAudio' -> Effect Unit

foreign import makeAllpass_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeBandpass_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeConstant_ :: Int -> Boolean -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeConvolver_ :: Int -> String -> FFIAudio' -> Effect Unit

foreign import makeDelay_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeDynamicsCompressor_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeGain_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeHighpass_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeHighshelf_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeLoopBuf_ :: Int -> String -> Boolean -> FFIAudioParameter' -> Number -> Number -> FFIAudio' -> Effect Unit

foreign import makeLowpass_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeLowshelf_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeMicrophone_ :: Int -> FFIAudio' -> Effect Unit

foreign import makeNotch_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makePeaking_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makePeriodicOsc_ :: Int -> String -> Boolean -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makePlayBuf_ :: Int -> String -> Number -> Boolean -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeRecorder_ :: Int -> String -> FFIAudio' -> Effect Unit

foreign import makeSawtoothOsc_ :: Int -> Boolean -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeSinOsc_ :: Int -> Boolean -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeSpeaker_ :: Int -> FFIAudio' -> Effect Unit

foreign import makeSquareOsc_ :: Int -> Boolean -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeStereoPanner_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeTriangleOsc_ :: Int -> Boolean -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import makeWaveShaper_ :: Int -> String -> String -> FFIAudio' -> Effect Unit

foreign import setOn_ :: Int -> FFIAudio' -> Effect Unit

foreign import setOff_ :: Int -> FFIAudio' -> Effect Unit

foreign import setLoopStart_ :: Int -> Number -> FFIAudio' -> Effect Unit

foreign import setLoopEnd_ :: Int -> Number -> FFIAudio' -> Effect Unit

foreign import setRatio_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setOffset_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setAttack_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setGain_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setQ_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setPan_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setThreshold_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setRelease_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setKnee_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setDelay_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setPlaybackRate_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

foreign import setFrequency_ :: Int -> FFIAudioParameter' -> FFIAudio' -> Effect Unit

instance effectfulAudioInterpret :: AudioInterpret FFIAudio (Effect Unit) where
  connectXToY a b c = connectXToY_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  disconnectXFromY a b c = disconnectXFromY_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  destroyUnit a b = destroyUnit_ (safeToFFI a) (safeToFFI b)
  rebaseAllUnits a b = rebaseAllUnits_ a (safeToFFI b)
  makeAllpass a b c d = makeAllpass_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeBandpass a b c d = makeBandpass_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeConstant a b c d = makeConstant_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeConvolver a b c = makeConvolver_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeDelay a b c = makeDelay_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeDynamicsCompressor a b c d e f g = makeDynamicsCompressor_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e) (safeToFFI f) (safeToFFI g)
  makeGain a b c = makeGain_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeHighpass a b c d = makeHighpass_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeHighshelf a b c d = makeHighshelf_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeLoopBuf a b c d e f g = makeLoopBuf_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e) (safeToFFI f) (safeToFFI g)
  makeLowpass a b c d = makeLowpass_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeLowshelf a b c d = makeLowshelf_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeMicrophone a b = makeMicrophone_ (safeToFFI a) (safeToFFI b)
  makeNotch a b c d = makeNotch_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makePeaking a b c d e = makePeaking_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e)
  makePeriodicOsc a b c d e = makePeriodicOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e)
  makePlayBuf a b c d e f = makePlayBuf_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e) (safeToFFI f)
  makeRecorder a b c = makeRecorder_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeSawtoothOsc a b c d = makeSawtoothOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeSinOsc a b c d = makeSinOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeSpeaker a b = makeSpeaker_ (safeToFFI a) (safeToFFI b)
  makeSquareOsc a b c d = makeSquareOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeStereoPanner a b c = makeStereoPanner_ (safeToFFI a) (safeToFFI b) (safeToFFI c)
  makeTriangleOsc a b c d = makeTriangleOsc_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeWaveShaper a b c d = makeWaveShaper_ (safeToFFI a) (safeToFFI b) (safeToFFI c) (safeToFFI d)
  setOn a b = setOn_ (safeToFFI a) (safeToFFI b)
  setOff a b = setOff_ (safeToFFI a) (safeToFFI b)
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

-- A utility typeclass used to convert PS arguments to arguments that are understood by the Web Audio API.
class SafeToFFI a b | a -> b where
  safeToFFI :: a -> b

instance safeToFFI_Int :: SafeToFFI Int Int where
  safeToFFI = identity

instance safeToFFI_Number :: SafeToFFI Number Number where
  safeToFFI = identity

instance safeToFFI_String :: SafeToFFI String String where
  safeToFFI = identity

instance safeToFFI_Oversample :: SafeToFFI Oversample String where
  safeToFFI = case _ of
    None -> "none"
    TwoX -> "2x"
    FourX -> "4x"

instance safeToFFI_OnOff :: SafeToFFI OnOff Boolean where
  safeToFFI = case _ of
    On -> true
    Off -> false

instance safeToFFI_FFIAudio :: SafeToFFI FFIAudio FFIAudio' where
  safeToFFI (FFIAudio x) = x

-- | An AudioParameter with the `transition` field stringly-typed for easier rendering in the FFI.
type FFIAudioParameter'
  = { param :: Number
    , timeOffset :: Number
    , transition :: String
    }

instance safeToFFI_AudioParameter ::
  SafeToFFI AudioParameter FFIAudioParameter' where
  safeToFFI (AudioParameter { param, timeOffset, transition }) =
    { param
    , timeOffset
    , transition: show transition
    }
