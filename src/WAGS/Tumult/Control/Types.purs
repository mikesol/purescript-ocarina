module WAGS.Tumult.Control.Types where


import WAGS.Core (Instruction)

newtype WAG  (graph :: Row Type) = WAG { instructions :: Array Instruction }
