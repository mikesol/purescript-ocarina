module WAGS.Example.KitchenSink.Types.PeriodicOsc where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec as V
import Data.Vec ((+>))
import Math (cos, pi, pow, sin, (%))
import WAGS.Change (ichange)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TPeriodicOsc)
import WAGS.Create.Optionals (CPeriodicOsc, periodicOsc)

type PeriodicOscGraph
  = TopWith { periodicOsc :: Unit }
      ( periodicOsc :: TPeriodicOsc /\ {}
      )

ksPeriodicOscCreate :: { periodicOsc :: CPeriodicOsc String }
ksPeriodicOscCreate = { periodicOsc: periodicOsc "my-wave" 440.0 }

deltaKsPeriodicOsc :: forall proof. Number -> IxWAGSig' PeriodicOscGraph PeriodicOscGraph proof Unit
deltaKsPeriodicOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksPeriodicOsc.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time

          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          if switchW then
            ichange
              { mix: 0.1 - 0.1 * (cos time)
              , periodicOsc:
                  { waveform: "my-wave"
                  , onOff: if switchOO then On else Off
                  , freq: 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)
                  }
              }
          else
            ichange
              { mix: 0.1 - 0.1 * (cos time)
              , periodicOsc:
                  { onOff: if switchOO then On else Off
                  , waveform: (0.1 +> -0.3 +> -0.5 +> 0.05 +> 0.2 +> V.empty) /\ (-0.05 +> 0.25 +> 0.4 +> -0.2 +> 0.05 +> V.empty)
                  , freq: 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)
                  }
              }
