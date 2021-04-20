module WAGS.Example.KitchenSink.Types.PeriodicOsc where

import Prelude

import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (phase3Integral, pieceTime)
import WAGS.Graph.Constructors (Gain(..), OnOff(..), PeriodicOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, defaultGetSetAP)
import WAGS.Universe.AudioUnit (TGain, TPeriodicOsc, TSpeaker)
import WAGS.Universe.Bin (D0, D1, D5, D6)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

----------- phase4
type PeriodicOscGraph
  = GraphC
      (NodeC (TPeriodicOsc D5 "my-wave") NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D5)) NodeListNil)
      )

type PeriodicOscUniverse cb
  = Universe' D6 PeriodicOscGraph cb

type Phase4 g t
  = Speaker (g (Gain GetSetAP (t (PeriodicOsc "my-wave" GetSetAP))))

phase4' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  Phase4 g t
phase4' fg ft =
  Speaker
    ( fg
        $ Gain
            (defaultGetSetAP 0.0)
            (ft $ PeriodicOsc (Proxy :: Proxy "my-wave") On (defaultGetSetAP 440.0))
    )

phase4 :: Phase4 Identity Identity
phase4 = phase4' Identity Identity

phase4PeriodicOsc :: Phase4 Identity Focus
phase4PeriodicOsc = phase4' Identity Focus

phase4Gain :: Phase4 Focus Identity
phase4Gain = phase4' Focus Identity

deltaPhase4 :: Number -> Speaker (Gain GetSetAP (PeriodicOsc "my-wave" GetSetAP))
deltaPhase4 = 
  (_ % pieceTime)
    >>> (_ - phase3Integral)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          Speaker $ Gain (defaultGetSetAP $ 0.1 - 0.1 * (cos time)) (PeriodicOsc (Proxy :: Proxy "my-wave") On (defaultGetSetAP $ 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))