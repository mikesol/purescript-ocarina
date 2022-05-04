module WAGS.Common.Parameters.DynamicsCompressor where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data DynamicsCompressorOptions = DynamicsCompressorOptions

instance
  ConvertOption DynamicsCompressorOptions
    "threshold"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DynamicsCompressorOptions
    "ratio"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DynamicsCompressorOptions
    "knee"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DynamicsCompressorOptions
    "attack"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DynamicsCompressorOptions
    "release"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

type DynamicsCompressorOptional =
  ( ratio :: Core.InitialAudioParameter
  , threshold :: Core.InitialAudioParameter
  , attack :: Core.InitialAudioParameter
  , release :: Core.InitialAudioParameter
  , knee :: Core.InitialAudioParameter
  )

type DynamicsCompressorAll =
  (| DynamicsCompressorOptional)

defaultDynamicsCompressor :: { | DynamicsCompressorOptional }
defaultDynamicsCompressor =
  { ratio: 12.0
  , attack: 0.003
  , release: 0.25
  , knee: 30.0
  , threshold: -24.0
  }

class InitialDynamicsCompressor i where
  toInitializeDynamicsCompressor :: i -> Core.InitializeDynamicsCompressor

instance InitialDynamicsCompressor Core.InitializeDynamicsCompressor where
  toInitializeDynamicsCompressor = identity

instance
  ConvertOptionsWithDefaults DynamicsCompressorOptions { | DynamicsCompressorOptional }
    { | provided }
    { | DynamicsCompressorAll } =>
  InitialDynamicsCompressor { | provided } where
  toInitializeDynamicsCompressor provided = Core.InitializeDynamicsCompressor
    (convertOptionsWithDefaults DynamicsCompressorOptions defaultDynamicsCompressor provided)
