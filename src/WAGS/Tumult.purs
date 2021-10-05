module WAGS.Tumult (Tumultuous, unsafeTumult, safeUntumult) where

import Prelude

import Data.Set (Set)
import WAGS.Rendered (Instruction)

newtype Tumultuous :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
newtype Tumultuous n terminus inputs = Tumultuous (Array (Set Instruction))

derive instance eqTumult :: Eq (Tumultuous n terminus inputs)
derive instance ordTumult :: Ord (Tumultuous n terminus inputs)
instance showTumult :: Show (Tumultuous n terminus inputs) where
  show (Tumultuous tumult) = "Tumult <" <> show tumult <> ">"

unsafeTumult :: forall n terminus inputs. Array (Set Instruction) -> Tumultuous n terminus inputs
unsafeTumult = Tumultuous

safeUntumult :: forall n terminus inputs. Tumultuous n terminus inputs -> Array (Set Instruction)
safeUntumult (Tumultuous tumult) = tumult