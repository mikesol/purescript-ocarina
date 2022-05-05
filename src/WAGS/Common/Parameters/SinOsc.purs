module WAGS.Common.Parameters.SinOsc where

import Prelude

import WAGS.Core as Core

class InitialSinOsc i where
  toInitializeSinOsc :: i -> Core.InitializeSinOsc

instance InitialSinOsc Core.InitializeSinOsc where
  toInitializeSinOsc = identity

instance InitialSinOsc Number where
  toInitializeSinOsc = Core.InitializeSinOsc <<< { frequency: _ }
