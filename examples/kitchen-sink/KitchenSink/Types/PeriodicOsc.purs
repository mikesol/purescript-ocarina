module WAGS.Example.KitchenSink.Types.PeriodicOsc where

import Prelude

import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (ksPeriodicOscIntegral, ksPeriodicOscTime, pieceTime)
import WAGS.Graph.Constructors (Gain(..), OnOff(..), PeriodicOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, defaultGetSetAP)
import WAGS.Universe.AudioUnit (TGain, TPeriodicOsc, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksPeriodicOscBegins = ksPeriodicOscIntegral - ksPeriodicOscTime :: Number

----------- ksPeriodicOsc
type PeriodicOscGraph
  = GraphC
      (NodeC (TPeriodicOsc D2 "my-wave") NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
      )

type PeriodicOscUniverse cb
  = Universe' D3 PeriodicOscGraph cb

type KsPeriodicOsc g t
  = Speaker (g (Gain GetSetAP (t (PeriodicOsc "my-wave" GetSetAP))))

ksPeriodicOsc' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  KsPeriodicOsc g t
ksPeriodicOsc' fg ft =
  Speaker
    ( fg
        $ Gain
            (defaultGetSetAP 0.0)
            (ft $ PeriodicOsc (Proxy :: Proxy "my-wave") On (defaultGetSetAP 440.0))
    )

ksPeriodicOsc :: KsPeriodicOsc Identity Identity
ksPeriodicOsc = ksPeriodicOsc' Identity Identity

ksPeriodicOscPeriodicOsc :: KsPeriodicOsc Identity Focus
ksPeriodicOscPeriodicOsc = ksPeriodicOsc' Identity Focus

ksPeriodicOscGain :: KsPeriodicOsc Focus Identity
ksPeriodicOscGain = ksPeriodicOsc' Focus Identity

deltaKsPeriodicOsc :: Number -> Speaker (Gain GetSetAP (PeriodicOsc "my-wave" GetSetAP))
deltaKsPeriodicOsc = 
  (_ % pieceTime)
    >>> (_ - ksPeriodicOscBegins)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          Speaker $ Gain (defaultGetSetAP $ 0.1 - 0.1 * (cos time)) (PeriodicOsc (Proxy :: Proxy "my-wave") On (defaultGetSetAP $ 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))