module WAGS.Example.KitchenSink.Types.SquareOsc where

import Prelude

import Data.Identity (Identity(..))
import WAGS.Control.Types (Universe')
import WAGS.Graph.Constructors (Gain(..), OnOff(..), Speaker(..), SquareOsc(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, defaultGetSetAP)
import WAGS.Universe.AudioUnit (TGain, TSpeaker, TSquareOsc)
import WAGS.Universe.Bin (D0, D1, D4, D5)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

----------- phase3
type SquareOscGraph
  = GraphC
      (NodeC (TSquareOsc D4) NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D4)) NodeListNil)
      )

type SquareOscUniverse cb
  = Universe' D5 SquareOscGraph cb

type Phase3 g t
  = Speaker (g (Gain GetSetAP (t (SquareOsc GetSetAP))))

phase3' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  Phase3 g t
phase3' fg ft =
  Speaker
    (fg $ Gain (defaultGetSetAP 0.0) (ft $ SquareOsc On (defaultGetSetAP 440.0)))

phase3 :: Phase3 Identity Identity
phase3 = phase3' Identity Identity

phase3SquareOsc :: Phase3 Identity Focus
phase3SquareOsc = phase3' Identity Focus

phase3Gain :: Phase3 Focus Identity
phase3Gain = phase3' Focus Identity

deltaPhase3 :: Number -> Speaker (Gain GetSetAP (SquareOsc GetSetAP))
deltaPhase3 time = Speaker $ Gain (defaultGetSetAP 0.0) (SquareOsc On (defaultGetSetAP 440.0))

phase3Time = 5.0 :: Number
