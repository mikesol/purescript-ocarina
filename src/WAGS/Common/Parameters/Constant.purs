module WAGS.Common.Parameters.Constant where

import Prelude

import WAGS.Core as Core

class InitialConstant i where
  toInitializeConstant :: i -> Core.InitializeConstant

instance InitialConstant Core.InitializeConstant where
  toInitializeConstant = identity

instance InitialConstant Number where
  toInitializeConstant = Core.InitializeConstant <<< { offset: _ }
