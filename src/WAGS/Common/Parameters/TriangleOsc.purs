module WAGS.Common.Parameters.TriangleOsc where

import Prelude

import WAGS.Core as Core

class InitialTriangleOsc i where
  toInitializeTriangleOsc :: i -> Core.InitializeTriangleOsc

instance InitialTriangleOsc Core.InitializeTriangleOsc where
  toInitializeTriangleOsc = identity

instance InitialTriangleOsc Number where
  toInitializeTriangleOsc = Core.InitializeTriangleOsc <<< { frequency: _ }
