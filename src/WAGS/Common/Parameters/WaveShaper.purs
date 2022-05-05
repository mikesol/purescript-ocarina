module WAGS.Common.Parameters.WaveShaper where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core
import WAGS.WebAPI (BrowserFloatArray)

data WaveShaperOptions = WaveShaperOptions

instance
  ConvertOption WaveShaperOptions
    "curve"
    BrowserFloatArray
    BrowserFloatArray where
  convertOption _ _ = identity

instance
  ConvertOption WaveShaperOptions
    "oversample"
    Core.Oversample
    Core.Oversample where
  convertOption _ _ = identity

type WaveShaperOptional =
  ( oversample :: Core.Oversample
  )

type WaveShaperAll =
  ( curve :: BrowserFloatArray
  | WaveShaperOptional
  )

defaultWaveShaper :: { | WaveShaperOptional }
defaultWaveShaper =
  { oversample: Core._twoX }

class InitialWaveShaper i where
  toInitializeWaveShaper :: i -> Core.InitializeWaveShaper

instance InitialWaveShaper Core.InitializeWaveShaper where
  toInitializeWaveShaper = identity

instance InitialWaveShaper BrowserFloatArray where
  toInitializeWaveShaper = toInitializeWaveShaper <<< { curve: _ }

instance
  ConvertOptionsWithDefaults WaveShaperOptions { | WaveShaperOptional } { | provided }
    { | WaveShaperAll } =>
  InitialWaveShaper { | provided } where
  toInitializeWaveShaper provided = Core.InitializeWaveShaper
    (convertOptionsWithDefaults WaveShaperOptions defaultWaveShaper provided)
