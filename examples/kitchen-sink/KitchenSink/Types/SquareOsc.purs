module WAGS.Example.KitchenSink.Types.SquareOsc where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (cos, pi, pow, sin, (%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CSquareOsc, squareOsc)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TSquareOsc)
import WAGS.Graph.Parameter (_off, _on)

type SquareOscGraph
  = TopWith { squareOsc :: Unit }
  ( squareOsc :: TSquareOsc /\ {}
  )

ksSquareOscCreate :: { squareOsc :: CSquareOsc }
ksSquareOscCreate = { squareOsc: squareOsc 220.0 }

deltaKsSquareOsc :: forall proof. Number -> IxWAGSig' SquareOscGraph SquareOscGraph proof Unit
deltaKsSquareOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksSquareOsc.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        rad = pi * time
        switchOO = time % 2.0 < 1.0
      in
        ichange
          { mix: 0.1 - 0.1 * (cos time)
          , squareOsc:
              { onOff: if switchOO then _on else _off
              , freq: 220.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)
              }
          }
