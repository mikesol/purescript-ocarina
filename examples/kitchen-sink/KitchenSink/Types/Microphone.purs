module WAGS.Example.KitchenSink.Types.Microphone where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%), cos, pi)
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CMicrophone, microphone)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TMicrophone)

type MicrophoneGraph
  = TopWith { microphone :: Unit }
      ( microphone :: TMicrophone /\ {}
      )

ksMicrophoneCreate :: { microphone :: CMicrophone }
ksMicrophoneCreate = { microphone: microphone }

deltaKsMicrophone :: forall proof. Number -> IxWAGSig' MicrophoneGraph MicrophoneGraph proof Unit
deltaKsMicrophone =
  (_ % pieceTime)
    >>> (_ - timing.ksMicrophone.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          ichange { mix: 0.1 - 0.1 * (cos time) }
