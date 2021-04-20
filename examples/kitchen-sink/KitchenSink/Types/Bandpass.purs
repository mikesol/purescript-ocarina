module WAGS.Example.KitchenSink.Types.Bandpass where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksBandpassIntegral, ksBandpassTime, pieceTime)
import WAGS.Graph.Constructors (Bandpass(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (TBandpass, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksBandpassBegins = ksBandpassIntegral - ksBandpassTime :: Number

type BandpassGraph
  = GraphC
      (NodeC (TBandpass D2) (SingleEdge D3))
      ( NodeListCons
          (NodeC (TPlayBuf D3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
          )
      )

type BandpassUniverse cb
  = Universe' D4 BandpassGraph cb

type KsBandpassreate (t :: Type -> Type) b
  = t (Bandpass Number Number (b (PlayBuf "my-buffer" Number)))

type KsBandpass g t b
  = Speaker (g (Gain Number (KsBandpassreate t b)))

ksBandpassCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsBandpassreate t b
ksBandpassCreate ft fb = ft $ Bandpass 300.0 1.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

ksBandpass' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsBandpass g t b
ksBandpass' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (ksBandpassCreate ft fb))

ksBandpass :: KsBandpass Identity Identity Identity
ksBandpass = ksBandpass' Identity Identity Identity

ksBandpassPlaybuf :: KsBandpass Identity Identity Focus
ksBandpassPlaybuf = ksBandpass' Identity Identity Focus

ksBandpassBandpass :: KsBandpass Identity Focus Identity
ksBandpassBandpass = ksBandpass' Identity Focus Identity

ksBandpassGain :: KsBandpass Focus Identity Identity
ksBandpassGain = ksBandpass' Focus Identity Identity

deltaKsBandpass :: Number -> KsBandpass Identity Identity Identity
deltaKsBandpass =
  (_ % pieceTime)
    >>> (_ - ksBandpassBegins)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Bandpass (calcSlope 0.0 300.0 ksBandpassTime 200.0 time) 1.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
