module WAGS.Example.KitchenSink.Types.SinOsc where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (cos, pi, pow, sin, (%))
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TSinOsc)
import WAGS.Graph.Optionals (CSinOsc, DSinOsc, DGain, gain_, sinOsc, sinOsc_)

type SinOscGraph
  = TopWith { sinOsc :: Unit }
      ( sinOsc :: TSinOsc /\ {}
      )

ksSinOscCreate :: { sinOsc :: CSinOsc }
ksSinOscCreate = { sinOsc: sinOsc  440.0 }

deltaKsSinOsc :: Number -> { mix :: DGain, sinOsc :: DSinOsc }
deltaKsSinOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksSinOsc.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time

          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (0.1 - 0.1 * (cos time))
          , sinOsc:
              sinOsc_ (if switchOO then On else Off) 
                (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
          }
