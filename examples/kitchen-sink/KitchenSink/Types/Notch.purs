module WAGS.Example.KitchenSink.Types.Notch where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksNotchIntegral, ksNotchTime, pieceTime)
import WAGS.Graph.Constructors (Notch(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (TNotch, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksNotchBegins = ksNotchIntegral - ksNotchTime :: Number

type NotchGraph
  = GraphC
      (NodeC (TNotch D2) (SingleEdge D3))
      ( NodeListCons
          (NodeC (TPlayBuf D3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
          )
      )

type NotchUniverse cb
  = Universe' D4 NotchGraph cb

type KsNotchreate (t :: Type -> Type) b
  = t (Notch Number Number (b (PlayBuf "my-buffer" Number)))

type KsNotch g t b
  = Speaker (g (Gain Number (KsNotchreate t b)))

ksNotchCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsNotchreate t b
ksNotchCreate ft fb = ft $ Notch 300.0 1.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

ksNotch' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsNotch g t b
ksNotch' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (ksNotchCreate ft fb))

ksNotch :: KsNotch Identity Identity Identity
ksNotch = ksNotch' Identity Identity Identity

ksNotchPlaybuf :: KsNotch Identity Identity Focus
ksNotchPlaybuf = ksNotch' Identity Identity Focus

ksNotchNotch :: KsNotch Identity Focus Identity
ksNotchNotch = ksNotch' Identity Focus Identity

ksNotchGain :: KsNotch Focus Identity Identity
ksNotchGain = ksNotch' Focus Identity Identity

deltaKsNotch :: Number -> KsNotch Identity Identity Identity
deltaKsNotch =
  (_ % pieceTime)
    >>> (_ - ksNotchBegins)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Notch (calcSlope 0.0 300.0 ksNotchTime 200.0 time) 1.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
