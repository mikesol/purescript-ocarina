module WAGS.Example.KitchenSink.Types.SawtoothOsc where

import Prelude

import Data.Identity (Identity(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Types.PeriodicOsc (phase4Integral)
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
deltaPhase5 time = Speaker $ Gain (defaultGetSetAP 0.0) (SawtoothOsc On (defaultGetSetAP 440.0))

phase5Time = 5.0 :: Number

phase5Integral = phase5Time + phase4Integral :: Number
