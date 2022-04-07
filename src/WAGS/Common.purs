module WAGS.Common where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Variant.Maybe (Maybe, just, nothing)
import WAGS.Core as C
import WAGS.Parameter (InitialAudioParameter)
import WAGS.WebAPI (BrowserAudioBuffer)


-- Gain
class InitialGain i where
  toInitializeGain :: i -> C.InitializeGain

instance InitialGain C.InitializeGain where
  toInitializeGain = identity

instance InitialGain Number where
  toInitializeGain = C.InitializeGain <<< { gain: _ }

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
  toInitialLoopBuf :: i -> C.InitializeLoopBuf

instance InitialLoopBuf C.InitializeLoopBuf where
  toInitialLoopBuf = identity

instance InitialLoopBuf BrowserAudioBuffer where
  toInitialLoopBuf = toInitialLoopBuf <<< { buffer: _ }

instance
  ConvertOptionsWithDefaults LoopBufOptions { | LoopBufOptional } { | provided }
    { | LoopBufAll } =>
  InitialLoopBuf { | provided } where
  toInitialLoopBuf provided = C.InitializeLoopBuf
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
  (  q :: InitialAudioParameter
  )

type LowpassAll =
  ( frequency :: InitialAudioParameter
  | LowpassOptional
  )

defaultLowpass :: { | LowpassOptional }
defaultLowpass =
  { q: 1.0 }

class InitialLowpass i where
  toInitialLowpass :: i -> C.InitializeLowpass

instance InitialLowpass C.InitializeLowpass where
  toInitialLowpass = identity

instance InitialLowpass InitialAudioParameter where
  toInitialLowpass = toInitialLowpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults LowpassOptions { | LowpassOptional } { | provided }
    { | LowpassAll } =>
  InitialLowpass { | provided } where
  toInitialLowpass provided = C.InitializeLowpass
    (convertOptionsWithDefaults LowpassOptions defaultLowpass provided)

-- SinOsc
class InitialSinOsc i where
  toInitializeSinOsc :: i -> C.InitializeSinOsc

instance InitialSinOsc C.InitializeSinOsc where
  toInitializeSinOsc = identity

instance InitialSinOsc Number where
  toInitializeSinOsc = C.InitializeSinOsc <<< { frequency: _ }
