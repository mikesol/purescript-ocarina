module WAGS.Example.KitchenSink.Types.Lowpass where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksLowpassIntegral, ksLowpassTime, pieceTime)
import WAGS.Graph.Constructors (Lowpass(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (TLowpass, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksLowpassBegins = ksLowpassIntegral - ksLowpassTime :: Number

type LowpassGraph
  = GraphC
      (NodeC (TLowpass D2) (SingleEdge D3))
      ( NodeListCons
          (NodeC (TPlayBuf D3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
          )
      )

type LowpassUniverse cb
  = Universe' D4 LowpassGraph cb

type KsLowpassreate (t :: Type -> Type) b
  = t (Lowpass Number Number (b (PlayBuf "my-buffer" Number)))

type KsLowpass g t b
  = Speaker (g (Gain Number (KsLowpassreate t b)))

ksLowpassCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsLowpassreate t b
ksLowpassCreate ft fb = ft $ Lowpass 300.0 1.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

ksLowpass' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsLowpass g t b
ksLowpass' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (ksLowpassCreate ft fb))

ksLowpass :: KsLowpass Identity Identity Identity
ksLowpass = ksLowpass' Identity Identity Identity

ksLowpassPlaybuf :: KsLowpass Identity Identity Focus
ksLowpassPlaybuf = ksLowpass' Identity Identity Focus

ksLowpassLowpass :: KsLowpass Identity Focus Identity
ksLowpassLowpass = ksLowpass' Identity Focus Identity

ksLowpassGain :: KsLowpass Focus Identity Identity
ksLowpassGain = ksLowpass' Focus Identity Identity

deltaKsLowpass :: Number -> KsLowpass Identity Identity Identity
deltaKsLowpass =
  (_ % pieceTime)
    >>> (_ - ksLowpassBegins)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Lowpass (calcSlope 0.0 300.0 ksLowpassTime 200.0 time) 1.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
