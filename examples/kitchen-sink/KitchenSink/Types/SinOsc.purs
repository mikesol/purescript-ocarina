module WAGS.Example.KitchenSink.Types.SinOsc where

import Prelude

import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime)
import WAGS.Graph.Constructors (Gain(..), OnOff(..), SinOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, defaultGetSetAP)
import WAGS.Universe.AudioUnit (TGain, TSinOsc, TSpeaker)
import WAGS.Universe.Bin (D0, D1, D2, D3)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

type SinOscGraph
  = GraphC
      (NodeC (TSinOsc D2) NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
      )

type SinOscUniverse cb
  = Universe' D3 SinOscGraph cb

type Phase1 g s
  = Speaker (g (Gain GetSetAP (s (SinOsc GetSetAP))))

phase1' ::
  forall g s.
  Decorating' g ->
  Decorating' s ->
  Phase1 g s
phase1' fg fs =
  Speaker
    (fg $ Gain (defaultGetSetAP 0.0) (fs $ SinOsc On (defaultGetSetAP 440.0)))

phase1 :: Phase1 Identity Identity
phase1 = phase1' Identity Identity

phase1SinOsc :: Phase1 Identity Focus
phase1SinOsc = phase1' Identity Focus

phase1Gain :: Phase1 Focus Identity
phase1Gain = phase1' Focus Identity

deltaPhase1 :: Number -> Speaker (Gain GetSetAP (SinOsc GetSetAP))
deltaPhase1 =
  (_ % pieceTime)
    >>> (_ - 0.0)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          Speaker $ Gain (defaultGetSetAP $ 0.1 - 0.1 * (cos time)) (SinOsc On (defaultGetSetAP $ 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))
