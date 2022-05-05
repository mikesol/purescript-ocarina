module WAGS.Common.Parameters.Microphone where

import Prelude

import WAGS.Core as Core
import WAGS.WebAPI (BrowserMicrophone)

class InitialMicrophone i where
  toInitializeMicrophone :: i -> Core.InitializeMicrophone

instance InitialMicrophone Core.InitializeMicrophone where
  toInitializeMicrophone = identity

instance InitialMicrophone BrowserMicrophone where
  toInitializeMicrophone = Core.InitializeMicrophone <<< { microphone: _ }
