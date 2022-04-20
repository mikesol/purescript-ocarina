module WAGS.Common where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos)
import Data.Variant (inj)
import Data.Variant.Maybe (Maybe, just, nothing)
import Data.Vec (Vec, toArray)
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, proof)
import Type.Proxy (Proxy(..))
import WAGS.Core (Oversample, PeriodicOscSpec(..), RealImg(..), _twoX)
import WAGS.Core as Core
import WAGS.Parameter (InitialAudioParameter)
import WAGS.WebAPI (BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb(..))

-- Allpass

data AllpassOptions = AllpassOptions

instance
  ConvertOption AllpassOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption AllpassOptions
    "q"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type AllpassOptional =
  ( q :: InitialAudioParameter
  )

type AllpassAll =
  ( frequency :: InitialAudioParameter
  | AllpassOptional
  )

defaultAllpass :: { | AllpassOptional }
defaultAllpass =
  { q: 1.0 }

class InitialAllpass i where
  toInitializeAllpass :: i -> Core.InitializeAllpass

instance InitialAllpass Core.InitializeAllpass where
  toInitializeAllpass = identity

instance InitialAllpass InitialAudioParameter where
  toInitializeAllpass = toInitializeAllpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults AllpassOptions { | AllpassOptional } { | provided }
    { | AllpassAll } =>
  InitialAllpass { | provided } where
  toInitializeAllpass provided = Core.InitializeAllpass
    (convertOptionsWithDefaults AllpassOptions defaultAllpass provided)

-- Bandpass

data BandpassOptions = BandpassOptions

instance
  ConvertOption BandpassOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption BandpassOptions
    "q"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type BandpassOptional =
  ( q :: InitialAudioParameter
  )

type BandpassAll =
  ( frequency :: InitialAudioParameter
  | BandpassOptional
  )

defaultBandpass :: { | BandpassOptional }
defaultBandpass =
  { q: 1.0 }

class InitialBandpass i where
  toInitializeBandpass :: i -> Core.InitializeBandpass

instance InitialBandpass Core.InitializeBandpass where
  toInitializeBandpass = identity

instance InitialBandpass InitialAudioParameter where
  toInitializeBandpass = toInitializeBandpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults BandpassOptions { | BandpassOptional }
    { | provided }
    { | BandpassAll } =>
  InitialBandpass { | provided } where
  toInitializeBandpass provided = Core.InitializeBandpass
    (convertOptionsWithDefaults BandpassOptions defaultBandpass provided)

-- Constant
class InitialConstant i where
  toInitializeConstant :: i -> Core.InitializeConstant

instance InitialConstant Core.InitializeConstant where
  toInitializeConstant = identity

instance InitialConstant Number where
  toInitializeConstant = Core.InitializeConstant <<< { offset: _ }

-- Convolver
class InitialConvolver i where
  toInitializeConvolver :: i -> Core.InitializeConvolver

instance InitialConvolver Core.InitializeConvolver where
  toInitializeConvolver = identity

instance InitialConvolver BrowserAudioBuffer where
  toInitializeConvolver = Core.InitializeConvolver <<< { buffer: _ }

-- IIRFilter
class InitialIIRFilter i feedforward feedback where
  toInitializeIIRFilter :: forall proxy. i -> proxy feedforward -> proxy feedback -> (Core.InitializeIIRFilter feedforward feedback)

instance
  ( TypeEquals feedforwardI feedforwardO
  , TypeEquals feedbackI feedbackO
  ) =>
  InitialIIRFilter (Core.InitializeIIRFilter feedforwardI feedbackI) feedforwardO feedbackO where
  toInitializeIIRFilter (Core.InitializeIIRFilter { feedforward, feedback }) _ _ = Core.InitializeIIRFilter
    { feedforward: proof (coerce feedforward), feedback: proof (coerce feedback) }

instance
  ( TypeEquals feedforwardI feedforwardO
  , TypeEquals feedbackI feedbackO
  ) =>
  InitialIIRFilter (Vec feedforwardI Number /\ Vec feedbackI Number) feedforwardO feedbackO where
  toInitializeIIRFilter (feedforward /\ feedback) _ _ = Core.InitializeIIRFilter { feedforward: proof (coerce feedforward), feedback: proof (coerce feedback) }

-- Delay
class InitialDelay i where
  toInitializeDelay :: i -> Core.InitializeDelay

instance InitialDelay Core.InitializeDelay where
  toInitializeDelay = identity

instance InitialDelay Number where
  toInitializeDelay = Core.InitializeDelay <<< { delayTime: _ }

-- DynamicsCompressor

data DynamicsCompressorOptions = DynamicsCompressorOptions

instance
  ConvertOption DynamicsCompressorOptions
    "threshold"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DynamicsCompressorOptions
    "ratio"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DynamicsCompressorOptions
    "knee"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DynamicsCompressorOptions
    "attack"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DynamicsCompressorOptions
    "release"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type DynamicsCompressorOptional =
  ( ratio :: InitialAudioParameter
  , threshold :: InitialAudioParameter
  , attack :: InitialAudioParameter
  , release :: InitialAudioParameter
  , knee :: InitialAudioParameter
  )

type DynamicsCompressorAll =
  (| DynamicsCompressorOptional)

defaultDynamicsCompressor :: { | DynamicsCompressorOptional }
defaultDynamicsCompressor =
  { ratio: 12.0
  , attack: 0.003
  , release: 0.25
  , knee: 30.0
  , threshold: -24.0
  }

class InitialDynamicsCompressor i where
  toInitializeDynamicsCompressor :: i -> Core.InitializeDynamicsCompressor

instance InitialDynamicsCompressor Core.InitializeDynamicsCompressor where
  toInitializeDynamicsCompressor = identity

instance
  ConvertOptionsWithDefaults DynamicsCompressorOptions { | DynamicsCompressorOptional }
    { | provided }
    { | DynamicsCompressorAll } =>
  InitialDynamicsCompressor { | provided } where
  toInitializeDynamicsCompressor provided = Core.InitializeDynamicsCompressor
    (convertOptionsWithDefaults DynamicsCompressorOptions defaultDynamicsCompressor provided)

-- Gain
class InitialGain i where
  toInitializeGain :: i -> Core.InitializeGain

instance InitialGain Core.InitializeGain where
  toInitializeGain = identity

instance InitialGain Number where
  toInitializeGain = Core.InitializeGain <<< { gain: _ }

-- Highpass

data HighpassOptions = HighpassOptions

instance
  ConvertOption HighpassOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption HighpassOptions
    "q"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type HighpassOptional =
  ( q :: InitialAudioParameter
  )

type HighpassAll =
  ( frequency :: InitialAudioParameter
  | HighpassOptional
  )

defaultHighpass :: { | HighpassOptional }
defaultHighpass =
  { q: 1.0 }

class InitialHighpass i where
  toInitializeHighpass :: i -> Core.InitializeHighpass

instance InitialHighpass Core.InitializeHighpass where
  toInitializeHighpass = identity

instance InitialHighpass InitialAudioParameter where
  toInitializeHighpass = toInitializeHighpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults HighpassOptions { | HighpassOptional }
    { | provided }
    { | HighpassAll } =>
  InitialHighpass { | provided } where
  toInitializeHighpass provided = Core.InitializeHighpass
    (convertOptionsWithDefaults HighpassOptions defaultHighpass provided)

-- Highshelf

data HighshelfOptions = HighshelfOptions

instance
  ConvertOption HighshelfOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption HighshelfOptions
    "gain"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type HighshelfOptional =
  ( gain :: InitialAudioParameter
  )

type HighshelfAll =
  ( frequency :: InitialAudioParameter
  | HighshelfOptional
  )

defaultHighshelf :: { | HighshelfOptional }
defaultHighshelf =
  { gain: 0.0 }

class InitialHighshelf i where
  toInitializeHighshelf :: i -> Core.InitializeHighshelf

instance InitialHighshelf Core.InitializeHighshelf where
  toInitializeHighshelf = identity

instance InitialHighshelf InitialAudioParameter where
  toInitializeHighshelf = toInitializeHighshelf <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults HighshelfOptions { | HighshelfOptional }
    { | provided }
    { | HighshelfAll } =>
  InitialHighshelf { | provided } where
  toInitializeHighshelf provided = Core.InitializeHighshelf
    (convertOptionsWithDefaults HighshelfOptions defaultHighshelf provided)

-- LoopBuf
data LoopBufOptions = LoopBufOptions

instance
  ConvertOption LoopBufOptions
    "playbackRate"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance ConvertOption LoopBufOptions "duration" Number (Maybe Number) where
  convertOption _ _ = just

instance ConvertOption LoopBufOptions "loopStart" Number Number where
  convertOption _ _ = identity

instance ConvertOption LoopBufOptions "loopEnd" Number Number where
  convertOption _ _ = identity

instance
  ConvertOption LoopBufOptions "buffer" BrowserAudioBuffer BrowserAudioBuffer where
  convertOption _ _ = identity

type LoopBufOptional =
  ( loopStart :: Number
  , loopEnd :: Number
  , playbackRate :: InitialAudioParameter
  , duration :: Maybe Number
  )

type LoopBufAll =
  ( buffer :: BrowserAudioBuffer
  | LoopBufOptional
  )

defaultLoopBuf :: { | LoopBufOptional }
defaultLoopBuf =
  { loopStart: 0.0
  , loopEnd: 0.0
  , playbackRate: 1.0
  , duration: nothing
  }

class InitialLoopBuf i where
  toInitializeLoopBuf :: i -> Core.InitializeLoopBuf

instance InitialLoopBuf Core.InitializeLoopBuf where
  toInitializeLoopBuf = identity

instance InitialLoopBuf BrowserAudioBuffer where
  toInitializeLoopBuf = toInitializeLoopBuf <<< { buffer: _ }

instance
  ConvertOptionsWithDefaults LoopBufOptions { | LoopBufOptional } { | provided }
    { | LoopBufAll } =>
  InitialLoopBuf { | provided } where
  toInitializeLoopBuf provided = Core.InitializeLoopBuf
    (convertOptionsWithDefaults LoopBufOptions defaultLoopBuf provided)

-- Lowpass
data LowpassOptions = LowpassOptions

instance
  ConvertOption LowpassOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption LowpassOptions
    "q"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type LowpassOptional =
  ( q :: InitialAudioParameter
  )

type LowpassAll =
  ( frequency :: InitialAudioParameter
  | LowpassOptional
  )

defaultLowpass :: { | LowpassOptional }
defaultLowpass =
  { q: 1.0 }

class InitialLowpass i where
  toInitializeLowpass :: i -> Core.InitializeLowpass

instance InitialLowpass Core.InitializeLowpass where
  toInitializeLowpass = identity

instance InitialLowpass InitialAudioParameter where
  toInitializeLowpass = toInitializeLowpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults LowpassOptions { | LowpassOptional } { | provided }
    { | LowpassAll } =>
  InitialLowpass { | provided } where
  toInitializeLowpass provided = Core.InitializeLowpass
    (convertOptionsWithDefaults LowpassOptions defaultLowpass provided)

-- Lowshelf

data LowshelfOptions = LowshelfOptions

instance
  ConvertOption LowshelfOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption LowshelfOptions
    "gain"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type LowshelfOptional =
  ( gain :: InitialAudioParameter
  )

type LowshelfAll =
  ( frequency :: InitialAudioParameter
  | LowshelfOptional
  )

defaultLowshelf :: { | LowshelfOptional }
defaultLowshelf =
  { gain: 0.0 }

class InitialLowshelf i where
  toInitializeLowshelf :: i -> Core.InitializeLowshelf

instance InitialLowshelf Core.InitializeLowshelf where
  toInitializeLowshelf = identity

instance InitialLowshelf InitialAudioParameter where
  toInitializeLowshelf = toInitializeLowshelf <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults LowshelfOptions { | LowshelfOptional }
    { | provided }
    { | LowshelfAll } =>
  InitialLowshelf { | provided } where
  toInitializeLowshelf provided = Core.InitializeLowshelf
    (convertOptionsWithDefaults LowshelfOptions defaultLowshelf provided)

-- Microphone
class InitialMicrophone i where
  toInitializeMicrophone :: i -> Core.InitializeMicrophone

instance InitialMicrophone Core.InitializeMicrophone where
  toInitializeMicrophone = identity

instance InitialMicrophone BrowserMicrophone where
  toInitializeMicrophone = Core.InitializeMicrophone <<< { microphone: _ }

-- IIRFilter

-- Notch

data NotchOptions = NotchOptions

instance
  ConvertOption NotchOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption NotchOptions
    "q"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type NotchOptional =
  ( q :: InitialAudioParameter
  )

type NotchAll =
  ( frequency :: InitialAudioParameter
  | NotchOptional
  )

defaultNotch :: { | NotchOptional }
defaultNotch =
  { q: 1.0 }

class InitialNotch i where
  toInitializeNotch :: i -> Core.InitializeNotch

instance InitialNotch Core.InitializeNotch where
  toInitializeNotch = identity

instance InitialNotch InitialAudioParameter where
  toInitializeNotch = toInitializeNotch <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults NotchOptions { | NotchOptional } { | provided }
    { | NotchAll } =>
  InitialNotch { | provided } where
  toInitializeNotch provided = Core.InitializeNotch
    (convertOptionsWithDefaults NotchOptions defaultNotch provided)

-- StereoPanner
class InitialStereoPanner i where
  toInitializeStereoPanner :: i -> Core.InitializeStereoPanner

instance InitialStereoPanner Core.InitializeStereoPanner where
  toInitializeStereoPanner = identity

instance InitialStereoPanner Number where
  toInitializeStereoPanner = Core.InitializeStereoPanner <<< { pan: _ }

-- Peaking

data PeakingOptions = PeakingOptions

instance
  ConvertOption PeakingOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption PeakingOptions
    "gain"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption PeakingOptions
    "q"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

type PeakingOptional =
  ( q :: InitialAudioParameter
  , gain :: InitialAudioParameter
  )

type PeakingAll =
  ( frequency :: InitialAudioParameter
  | PeakingOptional
  )

defaultPeaking :: { | PeakingOptional }
defaultPeaking =
  { q: 1.0, gain: 0.0 }

class InitialPeaking i where
  toInitializePeaking :: i -> Core.InitializePeaking

instance InitialPeaking Core.InitializePeaking where
  toInitializePeaking = identity

instance InitialPeaking InitialAudioParameter where
  toInitializePeaking = toInitializePeaking <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults PeakingOptions { | PeakingOptional } { | provided }
    { | PeakingAll } =>
  InitialPeaking { | provided } where
  toInitializePeaking provided = Core.InitializePeaking
    (convertOptionsWithDefaults PeakingOptions defaultPeaking provided)

-- PlayBuf
data PlayBufOptions = PlayBufOptions

instance
  ConvertOption PlayBufOptions
    "playbackRate"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance ConvertOption PlayBufOptions "duration" Number (Maybe Number) where
  convertOption _ _ = just

instance ConvertOption PlayBufOptions "bufferOffset" Number Number where
  convertOption _ _ = identity

instance
  ConvertOption PlayBufOptions "buffer" BrowserAudioBuffer BrowserAudioBuffer where
  convertOption _ _ = identity

type PlayBufOptional =
  ( bufferOffset :: Number
  , playbackRate :: InitialAudioParameter
  , duration :: Maybe Number
  )

type PlayBufAll =
  ( buffer :: BrowserAudioBuffer
  | PlayBufOptional
  )

defaultPlayBuf :: { | PlayBufOptional }
defaultPlayBuf =
  { bufferOffset: 0.0
  , playbackRate: 1.0
  , duration: nothing
  }

class InitialPlayBuf i where
  toInitializePlayBuf :: i -> Core.InitializePlayBuf

instance InitialPlayBuf Core.InitializePlayBuf where
  toInitializePlayBuf = identity

instance InitialPlayBuf BrowserAudioBuffer where
  toInitializePlayBuf = toInitializePlayBuf <<< { buffer: _ }

instance
  ConvertOptionsWithDefaults PlayBufOptions { | PlayBufOptional } { | provided }
    { | PlayBufAll } =>
  InitialPlayBuf { | provided } where
  toInitializePlayBuf provided = Core.InitializePlayBuf
    (convertOptionsWithDefaults PlayBufOptions defaultPlayBuf provided)

-- Periodic Osc
-- PeriodicOsc

data PeriodicOscOptions = PeriodicOscOptions

instance
  ConvertOption PeriodicOscOptions
    "frequency"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

class PeriodicOscSpecable i where
  toPeriodicOscSpec :: i -> PeriodicOscSpec

instance PeriodicOscSpecable BrowserPeriodicWave where
  toPeriodicOscSpec = PeriodicOscSpec <<< inj (Proxy :: _ "wave")

instance Pos n => PeriodicOscSpecable (Vec n Number /\ Vec n Number) where
  toPeriodicOscSpec (real /\ img) = PeriodicOscSpec $ inj (Proxy :: _ "realImg") $ RealImg { real: toArray real, img: toArray img }

instance PeriodicOscSpecable i =>
  ConvertOption PeriodicOscOptions
    "spec"
    i
    PeriodicOscSpec where
  convertOption _ _ = toPeriodicOscSpec


type PeriodicOscAll =
  ( frequency :: InitialAudioParameter
  , spec :: PeriodicOscSpec
  )

defaultPeriodicOsc :: { }
defaultPeriodicOsc = {  }

class InitialPeriodicOsc i where
  toInitializePeriodicOsc :: i -> Core.InitializePeriodicOsc

instance InitialPeriodicOsc Core.InitializePeriodicOsc where
  toInitializePeriodicOsc = identity

instance
  ConvertOptionsWithDefaults PeriodicOscOptions { } { | provided }
    { | PeriodicOscAll } =>
  InitialPeriodicOsc { | provided } where
  toInitializePeriodicOsc provided = Core.InitializePeriodicOsc
    (convertOptionsWithDefaults PeriodicOscOptions defaultPeriodicOsc provided)


-- SawtoothOsc
class InitialSawtoothOsc i where
  toInitializeSawtoothOsc :: i -> Core.InitializeSawtoothOsc

instance InitialSawtoothOsc Core.InitializeSawtoothOsc where
  toInitializeSawtoothOsc = identity

instance InitialSawtoothOsc Number where
  toInitializeSawtoothOsc = Core.InitializeSawtoothOsc <<< { frequency: _ }

-- SquareOsc
class InitialSquareOsc i where
  toInitializeSquareOsc :: i -> Core.InitializeSquareOsc

instance InitialSquareOsc Core.InitializeSquareOsc where
  toInitializeSquareOsc = identity

instance InitialSquareOsc Number where
  toInitializeSquareOsc = Core.InitializeSquareOsc <<< { frequency: _ }

-- Recorder
class InitialRecorder i where
  toInitializeRecorder :: i -> Core.InitializeRecorder

instance InitialRecorder Core.InitializeRecorder where
  toInitializeRecorder = identity

instance InitialRecorder MediaRecorderCb where
  toInitializeRecorder = Core.InitializeRecorder <<< { cb: _ }

-- SinOsc
class InitialSinOsc i where
  toInitializeSinOsc :: i -> Core.InitializeSinOsc

instance InitialSinOsc Core.InitializeSinOsc where
  toInitializeSinOsc = identity

instance InitialSinOsc Number where
  toInitializeSinOsc = Core.InitializeSinOsc <<< { frequency: _ }

-- TriangleOsc
class InitialTriangleOsc i where
  toInitializeTriangleOsc :: i -> Core.InitializeTriangleOsc

instance InitialTriangleOsc Core.InitializeTriangleOsc where
  toInitializeTriangleOsc = identity

instance InitialTriangleOsc Number where
  toInitializeTriangleOsc = Core.InitializeTriangleOsc <<< { frequency: _ }

-- WaveShaper

data WaveShaperOptions = WaveShaperOptions

instance
  ConvertOption WaveShaperOptions
    "curve"
    BrowserFloatArray
    BrowserFloatArray where
  convertOption _ _ = identity

instance
  ConvertOption WaveShaperOptions
    "oversample"
    Oversample
    Oversample where
  convertOption _ _ = identity

type WaveShaperOptional =
  ( oversample :: Oversample
  )

type WaveShaperAll =
  ( curve :: BrowserFloatArray
  | WaveShaperOptional
  )

defaultWaveShaper :: { | WaveShaperOptional }
defaultWaveShaper =
  { oversample: _twoX }

class InitialWaveShaper i where
  toInitializeWaveShaper :: i -> Core.InitializeWaveShaper

instance InitialWaveShaper Core.InitializeWaveShaper where
  toInitializeWaveShaper = identity

instance InitialWaveShaper BrowserFloatArray where
  toInitializeWaveShaper = toInitializeWaveShaper <<< { curve: _ }

instance
  ConvertOptionsWithDefaults WaveShaperOptions { | WaveShaperOptional } { | provided }
    { | WaveShaperAll } =>
  InitialWaveShaper { | provided } where
  toInitializeWaveShaper provided = Core.InitializeWaveShaper
    (convertOptionsWithDefaults WaveShaperOptions defaultWaveShaper provided)
