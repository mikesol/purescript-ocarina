module WAGS.Common.Parameters.Highpass where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data HighpassOptions = HighpassOptions

instance
  ConvertOption HighpassOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption HighpassOptions
    "q"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type HighpassOptional =
  ( q :: Core.InitialAudioParameter
  )

type HighpassAll =
  ( frequency :: Core.InitialAudioParameter
  | HighpassOptional
  )

defaultHighpass :: { | HighpassOptional }
defaultHighpass =
  { q: 1.0 }

class InitialHighpass i where
  toInitializeHighpass :: i -> Core.InitializeHighpass

instance InitialHighpass Core.InitializeHighpass where
  toInitializeHighpass = identity

instance InitialHighpass Core.InitialAudioParameter where
  toInitializeHighpass = toInitializeHighpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults HighpassOptions { | HighpassOptional }
    { | provided }
    { | HighpassAll } =>
  InitialHighpass { | provided } where
  toInitializeHighpass provided = Core.InitializeHighpass
    (convertOptionsWithDefaults HighpassOptions defaultHighpass provided)
