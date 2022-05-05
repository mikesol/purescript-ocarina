module WAGS.Common.Parameters.Highshelf where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data HighshelfOptions = HighshelfOptions

instance
  ConvertOption HighshelfOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption HighshelfOptions
    "gain"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type HighshelfOptional =
  ( gain :: Core.InitialAudioParameter
  )

type HighshelfAll =
  ( frequency :: Core.InitialAudioParameter
  | HighshelfOptional
  )

defaultHighshelf :: { | HighshelfOptional }
defaultHighshelf =
  { gain: 0.0 }

class InitialHighshelf i where
  toInitializeHighshelf :: i -> Core.InitializeHighshelf

instance InitialHighshelf Core.InitializeHighshelf where
  toInitializeHighshelf = identity

instance InitialHighshelf Core.InitialAudioParameter where
  toInitializeHighshelf = toInitializeHighshelf <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults HighshelfOptions { | HighshelfOptional }
    { | provided }
    { | HighshelfAll } =>
  InitialHighshelf { | provided } where
  toInitializeHighshelf provided = Core.InitializeHighshelf
    (convertOptionsWithDefaults HighshelfOptions defaultHighshelf provided)
