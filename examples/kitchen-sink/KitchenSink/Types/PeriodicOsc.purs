module WAGS.Example.KitchenSink.Types.PeriodicOsc where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (cos, pi, pow, sin, (%))
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TPeriodicOsc)
import WAGS.Graph.Optionals (CPeriodicOsc, DPeriodicOsc, DGain, gain_, periodicOsc, periodicOsc_)

type PeriodicOscGraph
  = TopWith { periodicOsc :: Unit }
      ( periodicOsc :: TPeriodicOsc /\ {}
      )

ksPeriodicOscCreate :: { periodicOsc :: CPeriodicOsc }
ksPeriodicOscCreate = { periodicOsc: periodicOsc "my-wave" 440.0 }

deltaKsPeriodicOsc :: Number -> { mix :: DGain, periodicOsc :: DPeriodicOsc }
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
          { mix: gain_ (0.1 - 0.1 * (cos time))
          , periodicOsc:
              periodicOsc_ (if switchOO then On else Off) (if switchW then "my-wave" else "another-wave")
                (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
          }
