module WAGS.Interpret where

import Prelude
import Control.Promise (Promise)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Typelevel.Num (class Pos)
import Data.Vec (Vec)
import Data.Vec as V
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import WAGS.Graph.Constructors (OnOff(..))
import WAGS.Graph.Parameter (AudioParameter(..))
import WAGS.Rendered (Instruction(..), Oversample(..))

foreign import data MediaRecorder :: Type

foreign import data BrowserPeriodicWave :: Type

foreign import data BrowserAudioBuffer :: Type

foreign import data BrowserFloatArray :: Type


foreign import data AudioContext :: Type

foreign import data BrowserMicrophone :: Type

foreign import getAudioClockTime :: AudioContext -> Effect Number

foreign import stopMediaRecorder :: MediaRecorder -> Effect Unit

foreign import mediaRecorderToUrl :: String -> (String -> Effect Unit) -> MediaRecorder -> Effect Unit

foreign import isTypeSupported :: String -> Effect Boolean

foreign import decodeAudioDataFromUri :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

foreign import decodeAudioDataFromBase64EncodedString :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

foreign import audioWorkletAddModule :: AudioContext -> String -> Effect (Promise Unit)

foreign import makeAudioContext :: Effect AudioContext

foreign import makePeriodicWaveImpl :: AudioContext -> Array Number -> Array Number -> Effect BrowserPeriodicWave

foreign import makeAudioBuffer :: AudioContext -> AudioBuffer -> Effect BrowserAudioBuffer

foreign import makeFloatArray :: Array Number -> Effect BrowserFloatArray

makePeriodicWave ::
  forall len.
  Pos len =>
  AudioContext ->
  Vec len Number ->
  Vec len Number ->
  Effect BrowserPeriodicWave
makePeriodicWave ctx a b = makePeriodicWaveImpl ctx (V.toArray a) (V.toArray b)

data AudioBuffer
  = AudioBuffer Int (Array (Array Number))

derive instance genericAudioBuffer :: Generic AudioBuffer _

instance showAudioBuffer :: Show AudioBuffer where
  show s = genericShow s

derive instance eqAudioBuffer :: Eq AudioBuffer

audioBuffer ::
  forall bch blen.
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  AudioBuffer
audioBuffer i v = AudioBuffer i (map V.toArray $ V.toArray v)

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

type FFIAudioParameter'
  = { param :: Number
    , timeOffset :: Number
    , transition :: String
    , forceSet :: Boolean
    }

instance safeToFFI_AudioParameter ::
  SafeToFFI AudioParameter FFIAudioParameter' where
  safeToFFI (AudioParameter { param, timeOffset, transition, forceSet }) =
    { param
    , timeOffset
    , transition: show transition
    , forceSet
    }

type FFIAudio'
  = { context :: AudioContext
    , writeHead :: Number
    , units :: Foreign
    , microphones :: Object BrowserMicrophone
    , recorders :: Object (MediaRecorder -> Effect Unit)
    , buffers :: Object BrowserAudioBuffer
    , floatArrays :: Object BrowserFloatArray
    , periodicWaves :: Object BrowserPeriodicWave
    }

foreign import renderAudio :: FFIAudio -> Array (FFIAudio -> Effect Unit) -> Effect Unit

newtype FFIAudio
  = FFIAudio FFIAudio'

class AudioInterpret audio engine where
  connectXToY :: Int -> Int -> audio -> engine
  disconnectXFromY :: Int -> Int -> audio -> engine
  destroyUnit :: Int -> audio -> engine
  rebaseAllUnits :: Array { from :: Int, to :: Int } -> audio -> engine
  makeAllpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  makeBandpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  makeConstant :: Int -> OnOff -> AudioParameter -> audio -> engine
  makeConvolver :: Int -> String -> audio -> engine
  makeDelay :: Int -> AudioParameter -> audio -> engine
  makeDynamicsCompressor :: Int -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine
  makeGain :: Int -> AudioParameter -> audio -> engine
  makeHighpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  makeHighshelf :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  makeLoopBuf :: Int -> String -> OnOff -> AudioParameter -> Number -> Number -> audio -> engine
  makeLowpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  makeLowshelf :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  makeMicrophone :: Int -> audio -> engine
  makeNotch :: Int -> AudioParameter -> AudioParameter -> audio -> engine
  makePeaking :: Int -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine
  makePeriodicOsc :: Int -> String -> OnOff -> AudioParameter -> audio -> engine
  makePlayBuf :: Int -> String -> Number -> OnOff -> AudioParameter -> audio -> engine
  makeRecorder :: Int -> String -> audio -> engine
  makeSawtoothOsc :: Int -> OnOff -> AudioParameter -> audio -> engine
  makeSinOsc :: Int -> OnOff -> AudioParameter -> audio -> engine
  makeSpeaker :: Int -> audio -> engine
  makeSquareOsc :: Int -> OnOff -> AudioParameter -> audio -> engine
  makeStereoPanner :: Int -> AudioParameter -> audio -> engine
  makeTriangleOsc :: Int -> OnOff -> AudioParameter -> audio -> engine
  makeWaveShaper :: Int -> String -> Oversample -> audio -> engine
  setOn :: Int -> audio -> engine
  setOff :: Int -> audio -> engine
  setLoopStart :: Int -> Number -> audio -> engine
  setLoopEnd :: Int -> Number -> audio -> engine
  setRatio :: Int -> AudioParameter -> audio -> engine
  setOffset :: Int -> AudioParameter -> audio -> engine
  setAttack :: Int -> AudioParameter -> audio -> engine
  setGain :: Int -> AudioParameter -> audio -> engine
  setQ :: Int -> AudioParameter -> audio -> engine
  setPan :: Int -> AudioParameter -> audio -> engine
  setThreshold :: Int -> AudioParameter -> audio -> engine
  setRelease :: Int -> AudioParameter -> audio -> engine
  setKnee :: Int -> AudioParameter -> audio -> engine
  setDelay :: Int -> AudioParameter -> audio -> engine
  setPlaybackRate :: Int -> AudioParameter -> audio -> engine
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
