module WAGS.Common.Parameters.StereoPanner where

import Prelude

import WAGS.Core as Core

class InitialStereoPanner i where
  toInitializeStereoPanner :: i -> Core.InitializeStereoPanner

instance InitialStereoPanner Core.InitializeStereoPanner where
  toInitializeStereoPanner = identity

instance InitialStereoPanner Number where
  toInitializeStereoPanner = Core.InitializeStereoPanner <<< { pan: _ }
