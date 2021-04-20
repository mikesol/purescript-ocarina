module WAGS.Example.KitchenSink.Types.Highshelf where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksHighshelfIntegral, ksHighshelfTime, pieceTime)
import WAGS.Graph.Constructors (Highshelf(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (THighshelf, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksHighshelfBegins = ksHighshelfIntegral - ksHighshelfTime :: Number

type HighshelfGraph
  = GraphC
      (NodeC (THighshelf D2) (SingleEdge D3))
      ( NodeListCons
          (NodeC (TPlayBuf D3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
          )
      )

type HighshelfUniverse cb
  = Universe' D4 HighshelfGraph cb

type KsHighshelfreate (t :: Type -> Type) b
  = t (Highshelf Number Number (b (PlayBuf "my-buffer" Number)))

type KsHighshelf g t b
  = Speaker (g (Gain Number (KsHighshelfreate t b)))

ksHighshelfCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsHighshelfreate t b
ksHighshelfCreate ft fb = ft $ Highshelf 300.0 0.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

ksHighshelf' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsHighshelf g t b
ksHighshelf' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (ksHighshelfCreate ft fb))

ksHighshelf :: KsHighshelf Identity Identity Identity
ksHighshelf = ksHighshelf' Identity Identity Identity

ksHighshelfPlaybuf :: KsHighshelf Identity Identity Focus
ksHighshelfPlaybuf = ksHighshelf' Identity Identity Focus

ksHighshelfHighshelf :: KsHighshelf Identity Focus Identity
ksHighshelfHighshelf = ksHighshelf' Identity Focus Identity

ksHighshelfGain :: KsHighshelf Focus Identity Identity
ksHighshelfGain = ksHighshelf' Focus Identity Identity

deltaKsHighshelf :: Number -> KsHighshelf Identity Identity Identity
deltaKsHighshelf =
  (_ % pieceTime)
    >>> (_ - ksHighshelfBegins)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Highshelf (calcSlope 0.0 300.0 ksHighshelfTime 200.0 time) 0.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
