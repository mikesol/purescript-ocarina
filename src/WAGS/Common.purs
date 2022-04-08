module WAGS.Common where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Variant.Maybe (Maybe, just, nothing)
import WAGS.Core as Core
import WAGS.Parameter (InitialAudioParameter)
import WAGS.WebAPI (BrowserAudioBuffer)

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
  toInitialAllpass :: i -> Core.InitializeAllpass

instance InitialAllpass Core.InitializeAllpass where
  toInitialAllpass = identity

instance InitialAllpass InitialAudioParameter where
  toInitialAllpass = toInitialAllpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults AllpassOptions { | AllpassOptional } { | provided }
    { | AllpassAll } =>
  InitialAllpass { | provided } where
  toInitialAllpass provided = Core.InitializeAllpass
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
  toInitialBandpass :: i -> Core.InitializeBandpass

instance InitialBandpass Core.InitializeBandpass where
  toInitialBandpass = identity

instance InitialBandpass InitialAudioParameter where
  toInitialBandpass = toInitialBandpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults BandpassOptions { | BandpassOptional }
    { | provided }
    { | BandpassAll } =>
  InitialBandpass { | provided } where
  toInitialBandpass provided = Core.InitializeBandpass
    (convertOptionsWithDefaults BandpassOptions defaultBandpass provided)

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
  toInitialHighpass :: i -> Core.InitializeHighpass

instance InitialHighpass Core.InitializeHighpass where
  toInitialHighpass = identity

instance InitialHighpass InitialAudioParameter where
  toInitialHighpass = toInitialHighpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults HighpassOptions { | HighpassOptional }
    { | provided }
    { | HighpassAll } =>
  InitialHighpass { | provided } where
  toInitialHighpass provided = Core.InitializeHighpass
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
  toInitialHighshelf :: i -> Core.InitializeHighshelf

instance InitialHighshelf Core.InitializeHighshelf where
  toInitialHighshelf = identity

instance InitialHighshelf InitialAudioParameter where
  toInitialHighshelf = toInitialHighshelf <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults HighshelfOptions { | HighshelfOptional }
    { | provided }
    { | HighshelfAll } =>
  InitialHighshelf { | provided } where
  toInitialHighshelf provided = Core.InitializeHighshelf
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
  toInitialLoopBuf :: i -> Core.InitializeLoopBuf

instance InitialLoopBuf Core.InitializeLoopBuf where
  toInitialLoopBuf = identity

instance InitialLoopBuf BrowserAudioBuffer where
  toInitialLoopBuf = toInitialLoopBuf <<< { buffer: _ }

instance
  ConvertOptionsWithDefaults LoopBufOptions { | LoopBufOptional } { | provided }
    { | LoopBufAll } =>
  InitialLoopBuf { | provided } where
  toInitialLoopBuf provided = Core.InitializeLoopBuf
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
  toInitialLowpass :: i -> Core.InitializeLowpass

instance InitialLowpass Core.InitializeLowpass where
  toInitialLowpass = identity

instance InitialLowpass InitialAudioParameter where
  toInitialLowpass = toInitialLowpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults LowpassOptions { | LowpassOptional } { | provided }
    { | LowpassAll } =>
  InitialLowpass { | provided } where
  toInitialLowpass provided = Core.InitializeLowpass
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
  toInitialLowshelf :: i -> Core.InitializeLowshelf

instance InitialLowshelf Core.InitializeLowshelf where
  toInitialLowshelf = identity

instance InitialLowshelf InitialAudioParameter where
  toInitialLowshelf = toInitialLowshelf <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults LowshelfOptions { | LowshelfOptional }
    { | provided }
    { | LowshelfAll } =>
  InitialLowshelf { | provided } where
  toInitialLowshelf provided = Core.InitializeLowshelf
    (convertOptionsWithDefaults LowshelfOptions defaultLowshelf provided)

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
  toInitialNotch :: i -> Core.InitializeNotch

instance InitialNotch Core.InitializeNotch where
  toInitialNotch = identity

instance InitialNotch InitialAudioParameter where
  toInitialNotch = toInitialNotch <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults NotchOptions { | NotchOptional } { | provided }
    { | NotchAll } =>
  InitialNotch { | provided } where
  toInitialNotch provided = Core.InitializeNotch
    (convertOptionsWithDefaults NotchOptions defaultNotch provided)

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
  toInitialPeaking :: i -> Core.InitializePeaking

instance InitialPeaking Core.InitializePeaking where
  toInitialPeaking = identity

instance InitialPeaking InitialAudioParameter where
  toInitialPeaking = toInitialPeaking <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults PeakingOptions { | PeakingOptional } { | provided }
    { | PeakingAll } =>
  InitialPeaking { | provided } where
  toInitialPeaking provided = Core.InitializePeaking
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

-- SinOsc
class InitialSinOsc i where
  toInitializeSinOsc :: i -> Core.InitializeSinOsc

instance InitialSinOsc Core.InitializeSinOsc where
  toInitializeSinOsc = identity

instance InitialSinOsc Number where
  toInitializeSinOsc = Core.InitializeSinOsc <<< { frequency: _ }
