module WAGS.Common.Parameters.Notch where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data NotchOptions = NotchOptions

instance
  ConvertOption NotchOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption NotchOptions
    "q"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type NotchOptional =
  ( q :: Core.InitialAudioParameter
  )

type NotchAll =
  ( frequency :: Core.InitialAudioParameter
  | NotchOptional
  )

defaultNotch :: { | NotchOptional }
defaultNotch =
  { q: 1.0 }

class InitialNotch i where
  toInitializeNotch :: i -> Core.InitializeNotch

instance InitialNotch Core.InitializeNotch where
  toInitializeNotch = identity

instance InitialNotch Core.InitialAudioParameter where
  toInitializeNotch = toInitializeNotch <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults NotchOptions { | NotchOptional } { | provided }
    { | NotchAll } =>
  InitialNotch { | provided } where
  toInitializeNotch provided = Core.InitializeNotch
    (convertOptionsWithDefaults NotchOptions defaultNotch provided)
