module WAGS.Example.KitchenSink.Types.SawtoothOsc where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (cos, pi, pow, sin, (%))
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TSawtoothOsc)
import WAGS.Graph.Optionals (CSawtoothOsc, DSawtoothOsc, DGain, gain_, sawtoothOsc, sawtoothOsc_)

type SawtoothOscGraph
  = TopWith { sawtoothOsc :: Unit }
      ( sawtoothOsc :: TSawtoothOsc /\ {}
      )

ksSawtoothOscCreate :: { sawtoothOsc :: CSawtoothOsc }
ksSawtoothOscCreate = { sawtoothOsc: sawtoothOsc  440.0 }

deltaKsSawtoothOsc :: Number -> { mix :: DGain, sawtoothOsc :: DSawtoothOsc }
deltaKsSawtoothOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksSawtoothOsc.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time

          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (0.1 - 0.1 * (cos time))
          , sawtoothOsc:
              sawtoothOsc_ (if switchOO then On else Off) 
                (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
          }
