module WAGS.Common where

import Prelude

import WAGS.Core as C


-- Gain
class InitialGain i where
  toInitializeGain :: i -> C.InitializeGain

instance InitialGain C.InitializeGain where
  toInitializeGain = identity

instance InitialGain Number where
  toInitializeGain = C.InitializeGain <<< { gain: _ }


-- SinOsc
class InitialSinOsc i where
  toInitializeSinOsc :: i -> C.InitializeSinOsc

instance InitialSinOsc C.InitializeSinOsc where
  toInitializeSinOsc = identity

instance InitialSinOsc Number where
  toInitializeSinOsc = C.InitializeSinOsc <<< { frequency: _ }
