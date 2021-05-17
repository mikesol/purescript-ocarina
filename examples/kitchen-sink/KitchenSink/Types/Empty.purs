module WAGS.Example.KitchenSink.Types.Empty where

import Prelude
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (TGain, TSpeaker)

type TopWith a b
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ a
    | b
    }

cursorGain = Proxy :: Proxy "mix"
