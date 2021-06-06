module WAGS.Example.KitchenSink.Types.SquareOsc where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (cos, pi, pow, sin, (%))
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TSquareOsc)
import WAGS.Create.Optionals (CSquareOsc, squareOsc)

type SquareOscGraph
  = TopWith { squareOsc :: Unit }
      ( squareOsc :: TSquareOsc /\ {}
      )

ksSquareOscCreate :: { squareOsc :: CSquareOsc }
ksSquareOscCreate = { squareOsc: squareOsc 220.0 }

deltaKsSquareOsc :: Number -> { mix :: DGain, squareOsc :: DSquareOsc }
deltaKsSquareOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksSquareOsc.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time

          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (0.1 - 0.1 * (cos time))
          , squareOsc:
              squareOsc_ (if switchOO then On else Off) 
                (220.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
          }
