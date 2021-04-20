module WAGS.Example.KitchenSink.Types.Peaking where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksPeakingIntegral, ksPeakingTime, pieceTime)
import WAGS.Graph.Constructors (Peaking(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (TPeaking, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksPeakingBegins = ksPeakingIntegral - ksPeakingTime :: Number

type PeakingGraph
  = GraphC
      (NodeC (TPeaking D2) (SingleEdge D3))
      ( NodeListCons
          (NodeC (TPlayBuf D3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
          )
      )

type PeakingUniverse cb
  = Universe' D4 PeakingGraph cb

type KsPeakingreate (t :: Type -> Type) b
  = t (Peaking Number Number Number (b (PlayBuf "my-buffer" Number)))

type KsPeaking g t b
  = Speaker (g (Gain Number (KsPeakingreate t b)))

ksPeakingCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsPeakingreate t b
ksPeakingCreate ft fb = ft $ Peaking 300.0 1.0 0.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

ksPeaking' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsPeaking g t b
ksPeaking' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (ksPeakingCreate ft fb))

ksPeaking :: KsPeaking Identity Identity Identity
ksPeaking = ksPeaking' Identity Identity Identity

ksPeakingPlaybuf :: KsPeaking Identity Identity Focus
ksPeakingPlaybuf = ksPeaking' Identity Identity Focus

ksPeakingPeaking :: KsPeaking Identity Focus Identity
ksPeakingPeaking = ksPeaking' Identity Focus Identity

ksPeakingGain :: KsPeaking Focus Identity Identity
ksPeakingGain = ksPeaking' Focus Identity Identity

deltaKsPeaking :: Number -> KsPeaking Identity Identity Identity
deltaKsPeaking =
  (_ % pieceTime)
    >>> (_ - ksPeakingBegins)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Peaking (calcSlope 0.0 300.0 ksPeakingTime 200.0 time) 1.0 0.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
