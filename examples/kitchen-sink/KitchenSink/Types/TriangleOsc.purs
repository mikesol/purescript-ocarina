module WAGS.Example.KitchenSink.Types.TriangleOsc where

import Prelude

import Data.Identity (Identity(..))
import WAGS.Control.Types (Universe')
import WAGS.Graph.Constructors (Gain(..), OnOff(..), Speaker(..), TriangleOsc(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, defaultGetSetAP)
import WAGS.Universe.AudioUnit (TGain, TSpeaker, TTriangleOsc)
import WAGS.Universe.Bin (D0, D1, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

----------- phase2
type TriangleOscGraph
  = GraphC
      (NodeC (TTriangleOsc D3) NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D3)) NodeListNil)
      )

type TriangleOscUniverse cb
  = Universe' D4 TriangleOscGraph cb

type Phase2 g t
  = Speaker (g (Gain GetSetAP (t (TriangleOsc GetSetAP))))

phase2' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  Phase2 g t
phase2' fg ft =
  Speaker
    (fg $ Gain (defaultGetSetAP 0.0) (ft $ TriangleOsc On (defaultGetSetAP 440.0)))

phase2 :: Phase2 Identity Identity
phase2 = phase2' Identity Identity

phase2TriangleOsc :: Phase2 Identity Focus
phase2TriangleOsc = phase2' Identity Focus

phase2Gain :: Phase2 Focus Identity
phase2Gain = phase2' Focus Identity

deltaPhase2 :: Number -> Speaker (Gain GetSetAP (TriangleOsc GetSetAP))
deltaPhase2 time = Speaker $ Gain (defaultGetSetAP 0.0) (TriangleOsc On (defaultGetSetAP 440.0))

phase2Time = 5.0 :: Number
