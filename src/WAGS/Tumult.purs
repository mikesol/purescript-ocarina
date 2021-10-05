module WAGS.Tumult (Tumultuous, unsafeTumult, safeUntumult) where

import Data.Set (Set)
import WAGS.Rendered (Instruction)

newtype Tumultuous :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
newtype Tumultuous n terminus inputs = Tumultuous (Array (Set Instruction))

unsafeTumult :: forall n terminus inputs. Array (Set Instruction) -> Tumultuous n terminus inputs
unsafeTumult = Tumultuous

safeUntumult :: forall n terminus inputs. Tumultuous n terminus inputs -> Array (Set Instruction)
safeUntumult (Tumultuous tumult) = tumult