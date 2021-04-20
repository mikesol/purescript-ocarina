module WAGS.Example.KitchenSink.Types.SinOsc where

import Prelude
import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (ksSinOscIntegral, ksSinOscTime, pieceTime)
import WAGS.Graph.Constructors (Gain, SinOsc, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, sinOsc, speaker)
import WAGS.Universe.AudioUnit (TGain, TSinOsc, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksSinOscBegins = ksSinOscIntegral - ksSinOscTime :: Number

type SinOscGraph
  = GraphC
      (NodeC (TSinOsc D2) NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
      )

type SinOscUniverse cb
  = Universe' D3 SinOscGraph cb

type KsSinOsc g s
  = Speaker (g (Gain GetSetAP (s (SinOsc GetSetAP))))

ksSinOsc' ::
  forall g s.
  Decorating' g ->
  Decorating' s ->
  KsSinOsc g s
ksSinOsc' fg fs = speaker (fg $ gain 0.0 (fs $ sinOsc 440.0))

ksSinOsc :: KsSinOsc Identity Identity
ksSinOsc = ksSinOsc' Identity Identity

ksSinOscSinOsc :: KsSinOsc Identity Focus
ksSinOscSinOsc = ksSinOsc' Identity Focus

ksSinOscGain :: KsSinOsc Focus Identity
ksSinOscGain = ksSinOsc' Focus Identity

deltaKsSinOsc :: Number -> Speaker (Gain GetSetAP (SinOsc GetSetAP))
deltaKsSinOsc =
  (_ % pieceTime)
    >>> (_ - ksSinOscBegins)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          speaker
            $ gain (0.1 - 0.1 * (cos time))
                (sinOsc (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))
