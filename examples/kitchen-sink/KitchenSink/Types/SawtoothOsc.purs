module WAGS.Example.KitchenSink.Types.SawtoothOsc where

import Prelude

import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (phase4Integral, pieceTime)
import WAGS.Graph.Constructors (Gain(..), OnOff(..), SawtoothOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, defaultGetSetAP)
import WAGS.Universe.AudioUnit (TGain, TSawtoothOsc, TSpeaker)
import WAGS.Universe.Bin (D0, D1, D6, D7)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

type SawtoothOscGraph
  = GraphC
      (NodeC (TSawtoothOsc D6) NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D6)) NodeListNil)
      )

type SawtoothOscUniverse cb
  = Universe' D7 SawtoothOscGraph cb

type Phase5 g t
  = Speaker (g (Gain GetSetAP (t (SawtoothOsc GetSetAP))))

phase5' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  Phase5 g t
phase5' fg ft =
  Speaker
    (fg $ Gain (defaultGetSetAP 0.0) (ft $ SawtoothOsc On (defaultGetSetAP 440.0)))

phase5 :: Phase5 Identity Identity
phase5 = phase5' Identity Identity

phase5SawtoothOsc :: Phase5 Identity Focus
phase5SawtoothOsc = phase5' Identity Focus

phase5Gain :: Phase5 Focus Identity
phase5Gain = phase5' Focus Identity

deltaPhase5 :: Number -> Speaker (Gain GetSetAP (SawtoothOsc GetSetAP))
deltaPhase5 =
  (_ % pieceTime)
    >>> (_ - phase4Integral)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          Speaker $ Gain (defaultGetSetAP $ 0.1 - 0.1 * (cos time)) (SawtoothOsc On (defaultGetSetAP $ 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))


