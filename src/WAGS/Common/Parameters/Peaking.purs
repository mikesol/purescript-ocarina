module WAGS.Common.Parameters.Peaking where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data PeakingOptions = PeakingOptions

instance
  ConvertOption PeakingOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption PeakingOptions
    "gain"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption PeakingOptions
    "q"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type PeakingOptional =
  ( q :: Core.InitialAudioParameter
  , gain :: Core.InitialAudioParameter
  )

type PeakingAll =
  ( frequency :: Core.InitialAudioParameter
  | PeakingOptional
  )

defaultPeaking :: { | PeakingOptional }
defaultPeaking =
  { q: 1.0, gain: 0.0 }

class InitialPeaking i where
  toInitializePeaking :: i -> Core.InitializePeaking

instance InitialPeaking Core.InitializePeaking where
  toInitializePeaking = identity

instance InitialPeaking Core.InitialAudioParameter where
  toInitializePeaking = toInitializePeaking <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults PeakingOptions { | PeakingOptional } { | provided }
    { | PeakingAll } =>
  InitialPeaking { | provided } where
  toInitializePeaking provided = Core.InitializePeaking
    (convertOptionsWithDefaults PeakingOptions defaultPeaking provided)
