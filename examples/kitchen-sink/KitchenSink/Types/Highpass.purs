module WAGS.Example.KitchenSink.Types.Highpass where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksHighpassIntegral, ksHighpassTime, pieceTime)
import WAGS.Graph.Constructors (Highpass(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (THighpass, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksHighpassBegins = ksHighpassIntegral - ksHighpassTime :: Number

type HighpassGraph
  = GraphC
      (NodeC (THighpass D2) (SingleEdge D3))
      ( NodeListCons
          (NodeC (TPlayBuf D3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
          )
      )

type HighpassUniverse cb
  = Universe' D4 HighpassGraph cb

type KsHighpassreate (t :: Type -> Type) b
  = t (Highpass Number Number (b (PlayBuf "my-buffer" Number)))

type KsHighpass g t b
  = Speaker (g (Gain Number (KsHighpassreate t b)))

ksHighpassCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsHighpassreate t b
ksHighpassCreate ft fb = ft $ Highpass 300.0 1.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

ksHighpass' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsHighpass g t b
ksHighpass' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (ksHighpassCreate ft fb))

ksHighpass :: KsHighpass Identity Identity Identity
ksHighpass = ksHighpass' Identity Identity Identity

ksHighpassPlaybuf :: KsHighpass Identity Identity Focus
ksHighpassPlaybuf = ksHighpass' Identity Identity Focus

ksHighpassHighpass :: KsHighpass Identity Focus Identity
ksHighpassHighpass = ksHighpass' Identity Focus Identity

ksHighpassGain :: KsHighpass Focus Identity Identity
ksHighpassGain = ksHighpass' Focus Identity Identity

deltaKsHighpass :: Number -> KsHighpass Identity Identity Identity
deltaKsHighpass =
  (_ % pieceTime)
    >>> (_ - ksHighpassBegins)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Highpass (calcSlope 0.0 300.0 ksHighpassTime 200.0 time) 1.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
