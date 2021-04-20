module WAGS.Example.KitchenSink.Types.Allpass where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksAllpassIntegral, ksAllpassTime, pieceTime)
import WAGS.Graph.Constructors (Allpass(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (TAllpass, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksAllpassBegins = ksAllpassIntegral - ksAllpassTime :: Number

type AllpassGraph
  = GraphC
      (NodeC (TAllpass D2) (SingleEdge D3))
      ( NodeListCons
          (NodeC (TPlayBuf D3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
          )
      )

type AllpassUniverse cb
  = Universe' D4 AllpassGraph cb

type KsAllpassCreate (t :: Type -> Type) b
  = t (Allpass Number Number (b (PlayBuf "my-buffer" Number)))

type KsAllpass g t b
  = Speaker (g (Gain Number (KsAllpassCreate t b)))

ksAllpassCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsAllpassCreate t b
ksAllpassCreate ft fb = ft $ Allpass 300.0 1.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

ksAllpass' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsAllpass g t b
ksAllpass' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (ksAllpassCreate ft fb))

ksAllpass :: KsAllpass Identity Identity Identity
ksAllpass = ksAllpass' Identity Identity Identity

ksAllpassPlaybuf :: KsAllpass Identity Identity Focus
ksAllpassPlaybuf = ksAllpass' Identity Identity Focus

ksAllpassAllpass :: KsAllpass Identity Focus Identity
ksAllpassAllpass = ksAllpass' Identity Focus Identity

ksAllpassGain :: KsAllpass Focus Identity Identity
ksAllpassGain = ksAllpass' Focus Identity Identity

deltaKsAllpass :: Number -> KsAllpass Identity Identity Identity
deltaKsAllpass =
  (_ % pieceTime)
    >>> (_ - ksAllpassBegins)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Allpass (calcSlope 0.0 300.0 ksAllpassTime 200.0 time) 1.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
