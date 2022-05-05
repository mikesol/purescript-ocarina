module WAGS.Common.Parameters.SawtoothOsc where

import Prelude

import WAGS.Core as Core

class InitialSawtoothOsc i where
  toInitializeSawtoothOsc :: i -> Core.InitializeSawtoothOsc

instance InitialSawtoothOsc Core.InitializeSawtoothOsc where
  toInitializeSawtoothOsc = identity

instance InitialSawtoothOsc Number where
  toInitializeSawtoothOsc = Core.InitializeSawtoothOsc <<< { frequency: _ }
