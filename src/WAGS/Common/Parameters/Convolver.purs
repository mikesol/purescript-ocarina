module WAGS.Common.Parameters.Convolver where

import Prelude

import WAGS.Core as Core
import WAGS.WebAPI (BrowserAudioBuffer)

class InitialConvolver i where
  toInitializeConvolver :: i -> Core.InitializeConvolver

instance InitialConvolver Core.InitializeConvolver where
  toInitializeConvolver = identity

instance InitialConvolver BrowserAudioBuffer where
  toInitializeConvolver = Core.InitializeConvolver <<< { buffer: _ }
