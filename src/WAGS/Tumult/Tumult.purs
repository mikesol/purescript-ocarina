module WAGS.Tumult.Tumult (Tumultuous, unsafeTumult, safeUntumult) where

import Prelude

import Data.Set (Set)
import WAGS.Core (Instruction)

newtype Tumultuous :: forall k1 k2. k1 -> k2 -> Type
newtype Tumultuous terminus inputs = Tumultuous (Set Instruction)

derive instance eqTumult :: Eq (Tumultuous terminus inputs)
derive instance ordTumult :: Ord (Tumultuous terminus inputs)

unsafeTumult
  :: forall terminus inputs
   . Set Instruction
  -> Tumultuous terminus inputs
unsafeTumult = Tumultuous

safeUntumult
  :: forall terminus inputs
   . Tumultuous terminus inputs
  -> Set Instruction
safeUntumult (Tumultuous tumult) = tumult