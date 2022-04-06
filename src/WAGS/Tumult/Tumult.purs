module WAGS.Tumult.Tumult (Tumultuous, unsafeTumult, safeUntumult) where

import Prelude

import WAGS.Core (Instruction)

newtype Tumultuous :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
newtype Tumultuous n terminus inputs = Tumultuous (Array (Array Instruction))

derive instance eqTumult :: Eq (Tumultuous n terminus inputs)
derive instance ordTumult :: Ord (Tumultuous n terminus inputs)

unsafeTumult
  :: forall n terminus inputs
   . Array (Array Instruction)
  -> Tumultuous n terminus inputs
unsafeTumult = Tumultuous

safeUntumult
  :: forall n terminus inputs
   . Tumultuous n terminus inputs
  -> Array (Array Instruction)
safeUntumult (Tumultuous tumult) = tumult