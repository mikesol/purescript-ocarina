module WAGS.Common.Parameters.Bandpass where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data BandpassOptions = BandpassOptions

instance
  ConvertOption BandpassOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption BandpassOptions
    "q"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type BandpassOptional =
  ( q :: Core.InitialAudioParameter
  )

type BandpassAll =
  ( frequency :: Core.InitialAudioParameter
  | BandpassOptional
  )

defaultBandpass :: { | BandpassOptional }
defaultBandpass =
  { q: 1.0 }

class InitialBandpass i where
  toInitializeBandpass :: i -> Core.InitializeBandpass

instance InitialBandpass Core.InitializeBandpass where
  toInitializeBandpass = identity

instance InitialBandpass Core.InitialAudioParameter where
  toInitializeBandpass = toInitializeBandpass <<< { frequency: _ }

instance
  ConvertOptionsWithDefaults BandpassOptions { | BandpassOptional }
    { | provided }
    { | BandpassAll } =>
  InitialBandpass { | provided } where
  toInitializeBandpass provided = Core.InitializeBandpass
    (convertOptionsWithDefaults BandpassOptions defaultBandpass provided)
