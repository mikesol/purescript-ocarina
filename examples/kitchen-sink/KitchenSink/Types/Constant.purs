module WAGS.Example.KitchenSink.Types.Constant where

import Prelude
import Data.Tuple.Nested (type (/\))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CConstant, constant)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TConstant)

type ConstantGraph
  = TopWith { constant :: Unit }
      ( constant :: TConstant /\ {}
      )

ksConstantCreate :: { constant :: CConstant }
ksConstantCreate = { constant: constant 0.0 }

deltaKsConstant :: forall proof. Number -> IxWAGSig' ConstantGraph ConstantGraph proof Unit
deltaKsConstant =
  const
    $ ichange
        { mix: 1.0
        , constant: 0.0
        }
