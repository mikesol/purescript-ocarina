module WAGS.Common.Parameters.Allpass where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data AllpassOptions = AllpassOptions

instance
  ConvertOption AllpassOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption AllpassOptions
    "q"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type AllpassOptional =
  ( q :: Core.InitialAudioParameter
  )

type AllpassAll =
  ( frequency :: Core.InitialAudioParameter
  | AllpassOptional
  )

defaultAllpass :: { | AllpassOptional }
defaultAllpass =
  { q: 1.0 }

class InitialAllpass i where
  toInitializeAllpass :: i -> Core.InitializeAllpass

instance InitialAllpass Core.InitializeAllpass where
  toInitializeAllpass = identity

instance InitialAllpass Core.InitialAudioParameter where
  toInitializeAllpass = toInitializeAllpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults AllpassOptions { | AllpassOptional } { | provided }
    { | AllpassAll } =>
  InitialAllpass { | provided } where
  toInitializeAllpass provided = Core.InitializeAllpass
    (convertOptionsWithDefaults AllpassOptions defaultAllpass provided)
