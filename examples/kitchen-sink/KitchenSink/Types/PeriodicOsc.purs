module WAGS.Example.KitchenSink.Types.PeriodicOsc where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec ((+>))
import Data.Vec as V
import Math (cos, pi, pow, sin, (%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CPeriodicOsc, periodicOsc)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TPeriodicOsc, _off, _on)
import WAGS.WebAPI (BrowserPeriodicWave)

type PeriodicOscGraph
  = TopWith { periodicOsc :: Unit }
  ( periodicOsc :: TPeriodicOsc /\ {}
  )

ksPeriodicOscCreate :: World -> { periodicOsc :: CPeriodicOsc BrowserPeriodicWave }
ksPeriodicOscCreate { periodicWaves: { "my-wave": myWave } } = { periodicOsc: periodicOsc myWave 440.0 }

deltaKsPeriodicOsc :: forall proof. World -> Number -> IxWAGSig' PeriodicOscGraph PeriodicOscGraph proof Unit
deltaKsPeriodicOsc { periodicWaves: { "my-wave": myWave } } =
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
                { waveform: myWave
                , onOff: if switchOO then _on else _off
                , freq: 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)
                }
            }
        else
          ichange
            { mix: 0.1 - 0.1 * (cos time)
            , periodicOsc:
                { onOff: if switchOO then _on else _off
                , waveform: (0.1 +> -0.3 +> -0.5 +> 0.05 +> 0.2 +> V.empty) /\ (-0.05 +> 0.25 +> 0.4 +> -0.2 +> 0.05 +> V.empty)
                , freq: 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)
                }
            }
