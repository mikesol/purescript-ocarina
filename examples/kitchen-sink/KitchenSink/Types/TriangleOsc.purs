module WAGS.Example.KitchenSink.Types.TriangleOsc where

import Prelude

import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2)
import WAGS.Graph.Constructors (Gain, Recorder, Speaker, TriangleOsc)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, recorder, speaker, triangleOsc)
import WAGS.Universe.AudioUnit (TRecorder, TTriangleOsc)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type TriangleOscGraph
  = GraphC
      (NodeC (TRecorder EI0 "my-recorder") (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TTriangleOsc EI1) NoEdge)
          (BaseGraph EI0)
      )

type TriangleOscUniverse cb
  = Universe' EI2 TriangleOscGraph cb

type KsTriangleOsc g t r
  = Speaker (g (Gain GetSetAP (r (Recorder "my-recorder" (t (TriangleOsc GetSetAP))))))

ksTriangleOsc' ::
  forall g t r.
  Decorating' g ->
  Decorating' t ->
  Decorating' r ->
  KsTriangleOsc g t r
ksTriangleOsc' fg ft fr = speaker (fg $ gain 0.0 (fr $ recorder (Proxy :: _ "my-recorder") (ft $ triangleOsc 440.0)))

ksTriangleOsc :: KsTriangleOsc Identity Identity Identity
ksTriangleOsc = ksTriangleOsc' Identity Identity Identity

ksTriangleOscTriangleOsc :: KsTriangleOsc Identity Focus Identity
ksTriangleOscTriangleOsc = ksTriangleOsc' Identity Focus Identity

ksTriangleOscGain :: KsTriangleOsc Focus Identity Identity
ksTriangleOscGain = ksTriangleOsc' Focus Identity Identity

ksTriangleOscRecorder :: KsTriangleOsc Identity Identity Focus
ksTriangleOscRecorder = ksTriangleOsc' Identity Identity Focus

deltaKsTriangleOsc :: Number -> Speaker (Gain GetSetAP (Recorder "my-recorder" (TriangleOsc GetSetAP)))
deltaKsTriangleOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksTriangleOsc.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          speaker
            $ gain (0.1 - 0.1 * (cos time))
                ( recorder (Proxy :: _ "my-recorder")
                    (triangleOsc (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))
                )
