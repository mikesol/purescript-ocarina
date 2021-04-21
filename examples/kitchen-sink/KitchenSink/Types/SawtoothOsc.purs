module WAGS.Example.KitchenSink.Types.SawtoothOsc where

import Prelude

import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1)
import WAGS.Graph.Constructors (Gain, SawtoothOsc, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, sawtoothOsc, speaker)
import WAGS.Universe.AudioUnit (TSawtoothOsc)
import WAGS.Universe.EdgeProfile (NoEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC)


type SawtoothOscGraph
  = GraphC
      (NodeC (TSawtoothOsc EI0) NoEdge)
      (BaseGraph EI0)

type SawtoothOscUniverse cb
  = Universe' EI1 SawtoothOscGraph cb

type KsSawtoothOsc g t
  = Speaker (g (Gain GetSetAP (t (SawtoothOsc GetSetAP))))

ksSawtoothOsc' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  KsSawtoothOsc g t
ksSawtoothOsc' fg ft = speaker (fg $ gain 0.0 (ft $ sawtoothOsc 440.0))

ksSawtoothOsc :: KsSawtoothOsc Identity Identity
ksSawtoothOsc = ksSawtoothOsc' Identity Identity

ksSawtoothOscSawtoothOsc :: KsSawtoothOsc Identity Focus
ksSawtoothOscSawtoothOsc = ksSawtoothOsc' Identity Focus

ksSawtoothOscGain :: KsSawtoothOsc Focus Identity
ksSawtoothOscGain = ksSawtoothOsc' Focus Identity

deltaKsSawtoothOsc :: Number -> Speaker (Gain GetSetAP (SawtoothOsc GetSetAP))
deltaKsSawtoothOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksSawtoothOsc.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          speaker
            $ gain (0.1 - 0.1 * (cos time))
                (sawtoothOsc (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))
