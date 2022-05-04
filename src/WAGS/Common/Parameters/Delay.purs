module WAGS.Common.Parameters.Delay where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core

data DelayOptions = DelayOptions

instance
  ConvertOption DelayOptions
    "delayTime"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption DelayOptions
    "maxDelayTime"
    Number
    Number where
  convertOption _ _ = identity

type DelayOptional =
  ( maxDelayTime :: Number
  )

type DelayAll =
  ( delayTime :: Core.InitialAudioParameter
  | DelayOptional
  )

defaultDelay :: { | DelayOptional }
defaultDelay =
  { maxDelayTime: 1.0 }

class InitialDelay i where
  toInitializeDelay :: i -> Core.InitializeDelay

instance InitialDelay Core.InitializeDelay where
  toInitializeDelay = identity

instance InitialDelay Core.InitialAudioParameter where
  toInitializeDelay = toInitializeDelay <<< { delayTime: _ }

instance
  ConvertOptionsWithDefaults DelayOptions { | DelayOptional }
    { | provided }
    { | DelayAll } =>
  InitialDelay { | provided } where
  toInitializeDelay provided = Core.InitializeDelay
    (convertOptionsWithDefaults DelayOptions defaultDelay provided)
