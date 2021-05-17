module WAGS.Example.KitchenSink.Types.Microphone where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%), cos, pi)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TMicrophone)
import WAGS.Graph.Optionals (CMicrophone, DGain, gain_, microphone)

type MicrophoneGraph
  = TopWith { microphone :: Unit }
      ( microphone :: TMicrophone /\ {}
      )

ksMicrophoneCreate :: { microphone :: CMicrophone }
ksMicrophoneCreate = { microphone: microphone }

deltaKsMicrophone :: Number -> { mix :: DGain }
deltaKsMicrophone =
  (_ % pieceTime)
    >>> (_ - timing.ksMicrophone.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          { mix: gain_ (0.1 - 0.1 * (cos time)) }
