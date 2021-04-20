module WAGS.Example.KitchenSink.Types.SawtoothOsc where

import Prelude

import Data.Identity (Identity(..))
import Math (cos, pi, pow, sin, (%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (ksSawtoothOscIntegral, ksSawtoothOscTime, pieceTime)
import WAGS.Graph.Constructors (Gain(..), OnOff(..), SawtoothOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, defaultGetSetAP)
import WAGS.Universe.AudioUnit (TGain, TSawtoothOsc, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksSawtoothOscBegins = ksSawtoothOscIntegral - ksSawtoothOscTime :: Number

type SawtoothOscGraph
  = GraphC
      (NodeC (TSawtoothOsc D2) NoEdge)
      ( NodeListCons
          (NodeC (TSpeaker D0) (SingleEdge D1))
          (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
      )

type SawtoothOscUniverse cb
  = Universe' D3 SawtoothOscGraph cb

type KsSawtoothOsc g t
  = Speaker (g (Gain GetSetAP (t (SawtoothOsc GetSetAP))))

ksSawtoothOsc' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  KsSawtoothOsc g t
ksSawtoothOsc' fg ft =
  Speaker
    (fg $ Gain (defaultGetSetAP 0.0) (ft $ SawtoothOsc On (defaultGetSetAP 440.0)))

ksSawtoothOsc :: KsSawtoothOsc Identity Identity
ksSawtoothOsc = ksSawtoothOsc' Identity Identity

ksSawtoothOscSawtoothOsc :: KsSawtoothOsc Identity Focus
ksSawtoothOscSawtoothOsc = ksSawtoothOsc' Identity Focus

ksSawtoothOscGain :: KsSawtoothOsc Focus Identity
ksSawtoothOscGain = ksSawtoothOsc' Focus Identity

deltaKsSawtoothOsc :: Number -> Speaker (Gain GetSetAP (SawtoothOsc GetSetAP))
deltaKsSawtoothOsc =
  (_ % pieceTime)
    >>> (_ - ksSawtoothOscBegins)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          Speaker $ Gain (defaultGetSetAP $ 0.1 - 0.1 * (cos time)) (SawtoothOsc On (defaultGetSetAP $ 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)))


