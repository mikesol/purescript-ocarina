module WAGS.Common.Parameters.Lowshelf where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data LowshelfOptions = LowshelfOptions

instance
  ConvertOption LowshelfOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption LowshelfOptions
    "gain"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type LowshelfOptional =
  ( gain :: Core.InitialAudioParameter
  )

type LowshelfAll =
  ( frequency :: Core.InitialAudioParameter
  | LowshelfOptional
  )

defaultLowshelf :: { | LowshelfOptional }
defaultLowshelf =
  { gain: 0.0 }

class InitialLowshelf i where
  toInitializeLowshelf :: i -> Core.InitializeLowshelf

instance InitialLowshelf Core.InitializeLowshelf where
  toInitializeLowshelf = identity

instance InitialLowshelf Core.InitialAudioParameter where
  toInitializeLowshelf = toInitializeLowshelf <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults LowshelfOptions { | LowshelfOptional }
    { | provided }
    { | LowshelfAll } =>
  InitialLowshelf { | provided } where
  toInitializeLowshelf provided = Core.InitializeLowshelf
    (convertOptionsWithDefaults LowshelfOptions defaultLowshelf provided)
