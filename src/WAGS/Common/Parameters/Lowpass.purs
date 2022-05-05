module WAGS.Common.Parameters.Lowpass where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data LowpassOptions = LowpassOptions

instance
  ConvertOption LowpassOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption LowpassOptions
    "q"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type LowpassOptional =
  ( q :: Core.InitialAudioParameter
  )

type LowpassAll =
  ( frequency :: Core.InitialAudioParameter
  | LowpassOptional
  )

defaultLowpass :: { | LowpassOptional }
defaultLowpass =
  { q: 1.0 }

class InitialLowpass i where
  toInitializeLowpass :: i -> Core.InitializeLowpass

instance InitialLowpass Core.InitializeLowpass where
  toInitializeLowpass = identity

instance InitialLowpass Core.InitialAudioParameter where
  toInitializeLowpass = toInitializeLowpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults LowpassOptions { | LowpassOptional }
    { | provided }
    { | LowpassAll } =>
  InitialLowpass { | provided } where
  toInitializeLowpass provided = Core.InitializeLowpass
    (convertOptionsWithDefaults LowpassOptions defaultLowpass provided)
