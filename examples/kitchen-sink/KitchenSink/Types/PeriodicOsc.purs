module WAGS.Example.KitchenSink.Types.PeriodicOsc where

import Prelude
import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, TopLevel)
import WAGS.Graph.Constructors (Gain, OnOff(..), PeriodicOsc, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, periodicOsc, speaker)
import WAGS.Universe.AudioUnit (TPeriodicOsc)
import WAGS.Universe.EdgeProfile (NoEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC)

----------- ksPeriodicOsc
type PeriodicOscGraph
  = GraphC
      (NodeC (TPeriodicOsc EI0) NoEdge)
      (BaseGraph EI0)

type PeriodicOscUniverse cb
  = Universe' EI1 PeriodicOscGraph cb

type KsPeriodicOsc g t
  = TopLevel g (t (PeriodicOsc GetSetAP))

ksPeriodicOsc' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  KsPeriodicOsc g t
ksPeriodicOsc' fg ft = speaker (fg $ gain 0.0 (ft $ periodicOsc "my-wave" 440.0))

ksPeriodicOsc :: KsPeriodicOsc Identity Identity
ksPeriodicOsc = ksPeriodicOsc' Identity Identity

ksPeriodicOscPeriodicOsc :: KsPeriodicOsc Identity Focus
ksPeriodicOscPeriodicOsc = ksPeriodicOsc' Identity Focus

ksPeriodicOscGain :: KsPeriodicOsc Focus Identity
ksPeriodicOscGain = ksPeriodicOsc' Focus Identity

deltaKsPeriodicOsc :: Number -> Speaker (Gain GetSetAP (PeriodicOsc GetSetAP))
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
          speaker
            $ gain (0.1 - 0.1 * (cos time))
                ( periodicOsc (if switchOO then On else Off) (if switchW then "my-wave" else "another-wave")
                    (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
                )
