module WAGS.Common.Parameters.Gain where

import Prelude

import WAGS.Core as Core

class InitialGain i where
  toInitializeGain :: i -> Core.InitializeGain

instance InitialGain Core.InitializeGain where
  toInitializeGain = identity

instance InitialGain Number where
  toInitializeGain = Core.InitializeGain <<< { gain: _ }
