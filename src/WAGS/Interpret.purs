-- | This documents the audio units exported by FRP.Behavior.Audio.
-- | The convention for audio units is the following:
-- |
-- | - highpass :: A highpass filter
-- | - highpassT :: A highpass filter using an AudioParameter, which has a temporal offset. 
-- | - highpass_ :: A named highpass filter.  Naming audio units speeds up computation a bit and may get rid of some artifacts.
-- | - highpassT_ :: A named highpass filter with a AudioParameters.
-- |
-- | All audio units have these four varieties.  Any audio unit that is not a generator takes one or many audio units as inputs.  In addition, some audio units (like `speaker` and `gain`) have a variety with an apostrophe (`speaker'` and `gain'`) that accept a single audio unit instead of a list.
module WAGS.Interpret where

import Prelude

import Control.Promise (Promise)
import Data.Const (Const(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Typelevel.Num (class Pos)
import Data.Vec (Vec)
import Data.Vec as V
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import WAGS.Graph.Parameter (AudioParameter(..))
import WAGS.Rendered (Instruction(..), Oversample(..))

foreign import data MediaRecorder :: Type

foreign import data BrowserPeriodicWave :: Type

foreign import data BrowserAudioBuffer :: Type

foreign import data BrowserFloatArray :: Type

foreign import data BrowserAudioTrack :: Type

foreign import data AudioContext :: Type

foreign import data BrowserMicrophone :: Type

foreign import stopMediaRecorder :: MediaRecorder -> Effect Unit

foreign import mediaRecorderToUrl :: String -> (String -> Effect Unit) -> MediaRecorder -> Effect Unit

foreign import isTypeSupported :: String -> Effect Boolean

foreign import decodeAudioDataFromUri :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

foreign import decodeAudioDataFromBase64EncodedString :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

foreign import audioWorkletAddModule :: AudioContext -> String -> Effect (Promise Unit)

foreign import makeAudioContext :: Effect AudioContext

foreign import makePeriodicWaveImpl :: AudioContext -> Array Number -> Array Number -> Effect BrowserPeriodicWave

foreign import makeAudioTrack :: String -> Effect BrowserAudioTrack

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

instance safeToFFI_FFIAudio :: SafeToFFI FFIAudio FFIAudio' where
  safeToFFI (FFIAudio x) = x

instance safeToFFI_FFIForeign :: SafeToFFI Foreign Foreign where
  safeToFFI = identity

instance safeToFFI_FFIEForeign :: SafeToFFI (Effect Foreign) (Effect Foreign) where
  safeToFFI = identity

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
  = { microphones :: Object BrowserMicrophone
    , recorders :: Object (MediaRecorder -> Effect Unit)
    , tracks :: Object BrowserAudioTrack
    , buffers :: Object BrowserAudioBuffer
    , floatArrays :: Object BrowserFloatArray
    , periodicWaves :: Object BrowserPeriodicWave
    }

newtype FFIAudio
  = FFIAudio FFIAudio'

class AudioInterpret audio engine where
  connectXToY :: Int -> Int -> audio -> engine audio
  disconnectXFromY :: Int -> Int -> audio -> engine audio
  destroyUnit :: Int -> audio -> engine audio
  rebaseAllUnits :: Array { from :: Int, to :: Int } -> audio -> engine audio
  makeAllpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeBandpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeConstant :: Int -> AudioParameter -> audio -> engine audio
  makeConvolver :: Int -> String -> audio -> engine audio
  makeDelay :: Int -> AudioParameter -> audio -> engine audio
  makeDynamicsCompressor :: Int -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeGain :: Int -> AudioParameter -> audio -> engine audio
  makeHighpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeHighshelf :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeLoopBuf :: Int -> String -> AudioParameter -> Number -> Number -> audio -> engine audio
  makeLowpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeLowshelf :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeMicrophone :: Int -> audio -> engine audio
  makeNotch :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makePeaking :: Int -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine audio
  makePeriodicOsc :: Int -> String -> AudioParameter -> audio -> engine audio
  makePlayBuf :: Int -> String -> Number -> AudioParameter -> audio -> engine audio
  makeRecorder :: Int -> String -> audio -> engine audio
  makeSawtoothOsc :: Int -> AudioParameter -> audio -> engine audio
  makeSinOsc :: Int -> AudioParameter -> audio -> engine audio
  makeSpeaker :: Int -> audio -> engine audio
  makeSquareOsc :: Int -> AudioParameter -> audio -> engine audio
  makeStereoPanner :: Int -> AudioParameter -> audio -> engine audio
  makeTriangleOsc :: Int -> AudioParameter -> audio -> engine audio
  makeWaveShaper :: Int -> String -> Oversample -> audio -> engine audio
  setLoopStart :: Int -> Number -> audio -> engine audio
  setLoopEnd :: Int -> Number -> audio -> engine audio
  setRatio :: Int -> AudioParameter -> audio -> engine audio
  setOffset :: Int -> AudioParameter -> audio -> engine audio
  setAttack :: Int -> AudioParameter -> audio -> engine audio
  setGain :: Int -> AudioParameter -> audio -> engine audio
  setQ :: Int -> AudioParameter -> audio -> engine audio
  setPan :: Int -> AudioParameter -> audio -> engine audio
  setThreshold :: Int -> AudioParameter -> audio -> engine audio
  setRelease :: Int -> AudioParameter -> audio -> engine audio
  setKnee :: Int -> AudioParameter -> audio -> engine audio
  setDelay :: Int -> AudioParameter -> audio -> engine audio
  setPlaybackRate :: Int -> AudioParameter -> audio -> engine audio
  setFrequency :: Int -> AudioParameter -> audio -> engine audio

instance freeAudioInterpret :: AudioInterpret Unit (Const Instruction) where
  connectXToY a b = const $ Const $ ConnectXToY a b
  disconnectXFromY a b = const $ Const $ DisconnectXFromY a b
  destroyUnit a = const $ Const $ DestroyUnit a
  rebaseAllUnits a = const $ Const $ RebaseAllUnits a
  makeAllpass a b c = const $ Const $ MakeAllpass a b c
  makeBandpass a b c = const $ Const $ MakeBandpass a b c
  makeConstant a b = const $ Const $ MakeConstant a b
  makeConvolver a b = const $ Const $ MakeConvolver a b
  makeDelay a b = const $ Const $ MakeDelay a b
  makeDynamicsCompressor a b c d e f = const $ Const $ MakeDynamicsCompressor a b c d e f
  makeGain a b = const $ Const $ MakeGain a b
  makeHighpass a b c = const $ Const $ MakeHighpass a b c
  makeHighshelf a b c = const $ Const $ MakeHighshelf a b c
  makeLoopBuf a b c d e = const $ Const $ MakeLoopBuf a b c d e
  makeLowpass a b c = const $ Const $ MakeLowpass a b c
  makeLowshelf a b c = const $ Const $ MakeLowshelf a b c
  makeMicrophone a = const $ Const $ MakeMicrophone a
  makeNotch a b c = const $ Const $ MakeNotch a b c
  makePeaking a b c d = const $ Const $ MakePeaking a b c d
  makePeriodicOsc a b c = const $ Const $ MakePeriodicOsc a b c
  makePlayBuf a b c d = const $ Const $ MakePlayBuf a b c d
  makeRecorder a b = const $ Const $ MakeRecorder a b
  makeSawtoothOsc a b = const $ Const $ MakeSawtoothOsc a b
  makeSinOsc a b = const $ Const $ MakeSinOsc a b
  makeSpeaker a = const $ Const $ MakeSpeaker a
  makeSquareOsc a b = const $ Const $ MakeSquareOsc a b
  makeStereoPanner a b = const $ Const $ MakeStereoPanner a b
  makeTriangleOsc a b = const $ Const $ MakeTriangleOsc a b
  makeWaveShaper a b c = const $ Const $ MakeWaveShaper a b c
  setLoopStart a b = const $ Const $ SetLoopStart a b
  setLoopEnd a b = const $ Const $ SetLoopEnd a b
  setRatio a b = const $ Const $ SetRatio a b
  setOffset a b = const $ Const $ SetOffset a b
  setAttack a b = const $ Const $ SetAttack a b
  setGain a b = const $ Const $ SetGain a b
  setQ a b = const $ Const $ SetQ a b
  setPan a b = const $ Const $ SetPan a b
  setThreshold a b = const $ Const $ SetThreshold a b
  setRelease a b = const $ Const $ SetRelease a b
  setKnee a b = const $ Const $ SetKnee a b
  setDelay a b = const $ Const $ SetDelay a b
  setPlaybackRate a b = const $ Const $ SetPlaybackRate a b
  setFrequency a b = const $ Const $ SetFrequency a b

foreign import connectXToY_ :: Int -> Int -> Foreign -> Effect Foreign

foreign import disconnectXFromY_ :: Int -> Int -> Foreign -> Effect Foreign

foreign import destroyUnit_ :: Int -> Foreign -> Effect Foreign

foreign import rebaseAllUnits_ :: Array { from :: Int, to :: Int } -> Foreign -> Effect Foreign

foreign import makeAllpass_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeBandpass_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeConstant_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeConvolver_ :: Int -> String -> Foreign -> Effect Foreign

foreign import makeDelay_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeDynamicsCompressor_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeGain_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeHighpass_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeHighshelf_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeLoopBuf_ :: Int -> String -> FFIAudioParameter' -> Number -> Number -> Foreign -> Effect Foreign

foreign import makeLowpass_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeLowshelf_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeMicrophone_ :: Int -> Foreign -> Effect Foreign

foreign import makeNotch_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makePeaking_ :: Int -> FFIAudioParameter' -> FFIAudioParameter' -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makePeriodicOsc_ :: Int -> String -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makePlayBuf_ :: Int -> String -> Number -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeRecorder_ :: Int -> String -> Foreign -> Effect Foreign

foreign import makeSawtoothOsc_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeSinOsc_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeSpeaker_ :: Int -> Foreign -> Effect Foreign

foreign import makeSquareOsc_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeStereoPanner_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeTriangleOsc_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import makeWaveShaper_ :: Int -> String -> Oversample -> Foreign -> Effect Foreign

foreign import setLoopStart_ :: Int -> Number -> Foreign -> Effect Foreign

foreign import setLoopEnd_ :: Int -> Number -> Foreign -> Effect Foreign

foreign import setRatio_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setOffset_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setAttack_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setGain_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setQ_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setPan_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setThreshold_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setRelease_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setKnee_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setDelay_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setPlaybackRate_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

foreign import setFrequency_ :: Int -> FFIAudioParameter' -> Foreign -> Effect Foreign

instance effectfulAudioInterpret :: AudioInterpret Foreign Effect where
  connectXToY a b c = connectXToY_ a b c
  disconnectXFromY a b c = disconnectXFromY_ a b c
  destroyUnit a b = destroyUnit_ a b
  rebaseAllUnits a = rebaseAllUnits_ a
  makeAllpass a b c = makeAllpass_ a (safeToFFI b) (safeToFFI c)
  makeBandpass a b c = makeBandpass_ a (safeToFFI b) (safeToFFI c)
  makeConstant a b c = makeConstant_ a (safeToFFI b) (safeToFFI c)
  makeConvolver = makeConvolver_
  makeDelay a b = makeDelay_ a (safeToFFI b)
  makeDynamicsCompressor a b c d e f = makeDynamicsCompressor_ a (safeToFFI b) (safeToFFI c) (safeToFFI d) (safeToFFI e) (safeToFFI f)
  makeGain a b = makeGain_ a (safeToFFI b)
  makeHighpass a b c = makeHighpass_ a (safeToFFI b) (safeToFFI c)
  makeHighshelf a b c = makeHighshelf_ a (safeToFFI b) (safeToFFI c)
  makeLoopBuf a b c = makeLoopBuf_ a (safeToFFI b) (safeToFFI c)
  makeLowpass a b c = makeLowpass_ a (safeToFFI b) (safeToFFI c)
  makeLowshelf a b c = makeLowshelf_ a (safeToFFI b) (safeToFFI c)
  makeMicrophone = makeMicrophone_
  makeNotch a b c = makeNotch_ a (safeToFFI b) (safeToFFI c)
  makePeaking a b c d = makePeaking_ a (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makePeriodicOsc a b c = makePeriodicOsc_ a (safeToFFI b) (safeToFFI c)
  makePlayBuf a b c d = makePlayBuf_ a (safeToFFI b) (safeToFFI c) (safeToFFI d)
  makeRecorder = makeRecorder_
  makeSawtoothOsc a b = makeSawtoothOsc_ a (safeToFFI b)
  makeSinOsc a b = makeSinOsc_ a (safeToFFI b)
  makeSpeaker = makeSpeaker_
  makeSquareOsc a b = makeSquareOsc_ a (safeToFFI b)
  makeStereoPanner a b = makeStereoPanner_ a (safeToFFI b)
  makeTriangleOsc a b = makeTriangleOsc_ a (safeToFFI b)
  makeWaveShaper = makeWaveShaper_
  setLoopStart = setLoopStart_
  setLoopEnd = setLoopEnd_
  setRatio a b = setRatio_ a (safeToFFI b)
  setOffset a b = setOffset_ a (safeToFFI b)
  setAttack a b = setAttack_ a (safeToFFI b)
  setGain a b = setGain_ a (safeToFFI b)
  setQ a b = setQ_ a (safeToFFI b)
  setPan a b = setPan_ a (safeToFFI b)
  setThreshold a b = setThreshold_ a (safeToFFI b)
  setRelease a b = setRelease_ a (safeToFFI b)
  setKnee a b = setKnee_ a (safeToFFI b)
  setDelay a b = setDelay_ a (safeToFFI b)
  setPlaybackRate a b = setPlaybackRate_ a (safeToFFI b)
  setFrequency a b = setFrequency_ a (safeToFFI b)
