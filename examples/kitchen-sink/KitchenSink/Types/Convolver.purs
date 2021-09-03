module WAGS.Example.KitchenSink.Types.Convolver where

import Prelude
import Data.Tuple.Nested (type (/\))
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TConvolver, TPlayBuf)

type ConvolverGraph
  = TopWith { convolver :: Unit }
  ( convolver :: TConvolver /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )
