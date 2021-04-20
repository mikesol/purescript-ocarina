module WAGS.Example.KitchenSink.Types.TriangleOsc where

import Prelude
import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (ksTriangleOscIntegral, ksTriangleOscTime, pieceTime)
import WAGS.Graph.Constructors (Gain, Speaker, TriangleOsc)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, speaker, triangleOsc)
import WAGS.Universe.AudioUnit (TGain, TSpeaker, TTriangleOsc)
import WAGS.Universe.BinN (D0, D1, D2, D3)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksTriangleOscBegin = ksTriangleOscIntegral - ksTriangleOscTime :: Number

type TriangleOscGraph
  = GraphC
      (NodeC (TTriangleOsc D2) NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
      )

type TriangleOscUniverse cb
  = Universe' D3 TriangleOscGraph cb

type KsTriangleOsc g t
  = Speaker (g (Gain GetSetAP (t (TriangleOsc GetSetAP))))

ksTriangleOsc' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  KsTriangleOsc g t
ksTriangleOsc' fg ft = speaker (fg $ gain 0.0 (ft $ triangleOsc 440.0))

ksTriangleOsc :: KsTriangleOsc Identity Identity
ksTriangleOsc = ksTriangleOsc' Identity Identity

ksTriangleOscTriangleOsc :: KsTriangleOsc Identity Focus
ksTriangleOscTriangleOsc = ksTriangleOsc' Identity Focus

ksTriangleOscGain :: KsTriangleOsc Focus Identity
ksTriangleOscGain = ksTriangleOsc' Focus Identity

deltaKsTriangleOsc :: Number -> Speaker (Gain GetSetAP (TriangleOsc GetSetAP))
deltaKsTriangleOsc =
  (_ % pieceTime)
    >>> (_ - ksTriangleOscBegin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          speaker
            $ gain (0.1 - 0.1 * (cos time))
                (triangleOsc (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))
