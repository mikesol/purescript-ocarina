module WAGS.Example.KitchenSink.Types.Constant where

import Prelude
import Data.Tuple.Nested (type (/\))
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TConstant)
import WAGS.Create.Optionals (CConstant, constant)

type ConstantGraph
  = TopWith { constant :: Unit }
      ( constant :: TConstant /\ {}
      )

ksConstantCreate :: { constant :: CConstant }
ksConstantCreate = { constant: constant 0.0 }

deltaKsConstant :: Number -> { mix :: DGain, constant :: DConstant }
deltaKsConstant =
  const
    { mix: gain_ 1.0
    , constant: constant_ 0.0
    }
