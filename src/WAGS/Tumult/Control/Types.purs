module WAGS.Tumult.Control.Types where


import Data.Set (Set)
import WAGS.Core (Instruction)

newtype WAG  (graph :: Row Type) = WAG { instructions :: Set Instruction }
