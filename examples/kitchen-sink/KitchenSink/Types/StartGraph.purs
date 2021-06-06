module WAGS.Example.KitchenSink.Types.StartGraph where

import Prelude

import Data.Tuple.Nested (type (/\))
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TSinOsc)

type StartGraph
  = TopWith { sinOsc :: Unit }
      ( sinOsc :: TSinOsc /\ {}
      )