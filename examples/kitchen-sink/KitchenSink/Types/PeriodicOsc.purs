module WAGS.Example.KitchenSink.Types.PeriodicOsc where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec as V
import Data.Vec ((+>))
import Math (cos, pi, pow, sin, (%))
import WAGS.Change (ichange)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TPeriodicOsc)
import WAGS.Graph.Optionals (CPeriodicOsc, gain_, periodicOsc, periodicOsc_)

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
            ivoid
              $ ichange
                  { mix: gain_ (0.1 - 0.1 * (cos time))
                  , periodicOsc:
                      periodicOsc_ (if switchOO then On else Off) "my-wave"
                        (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
                  }
          else
            ivoid
              $ ichange
                  { mix: gain_ (0.1 - 0.1 * (cos time))
                  , periodicOsc:
                      periodicOsc_ (if switchOO then On else Off) ((0.1 +> -0.3 +> -0.5 +> 0.05 +> 0.2 +> V.empty) /\ (-0.05 +> 0.25 +> 0.4 +> -0.2 +> 0.05 +> V.empty))
                        (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
                  }
