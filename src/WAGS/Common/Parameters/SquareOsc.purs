module WAGS.Common.Parameters.SquareOsc where

import Prelude

import WAGS.Core as Core

class InitialSquareOsc i where
  toInitializeSquareOsc :: i -> Core.InitializeSquareOsc

instance InitialSquareOsc Core.InitializeSquareOsc where
  toInitializeSquareOsc = identity

instance InitialSquareOsc Number where
  toInitializeSquareOsc = Core.InitializeSquareOsc <<< { frequency: _ }
