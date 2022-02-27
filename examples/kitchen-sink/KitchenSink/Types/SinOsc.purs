module WAGS.Example.KitchenSink.Types.SinOsc where

import Prelude

import Math (cos, pi, pow, sin, (%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CSinOsc, sinOsc)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.StartGraph (StartGraph)
import WAGS.Graph.Parameter (_off, _on)

type SinOscGraph
  = StartGraph

ksSinOscCreate :: { sinOsc :: CSinOsc }
ksSinOscCreate = { sinOsc: sinOsc 440.0 }

deltaKsSinOsc :: forall proof. Number -> IxWAGSig' SinOscGraph SinOscGraph proof Unit
deltaKsSinOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksSinOsc.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        rad = pi * time
        switchOO = time % 2.0 < 1.0
      in
        ichange
          { mix: 0.1 - 0.1 * (cos time)
          , sinOsc: { onOff: if switchOO then _on else _off, freq: 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0) }
          }
