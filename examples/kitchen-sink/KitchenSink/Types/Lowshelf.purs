module WAGS.Example.KitchenSink.Types.Lowshelf where

import Prelude
import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksLowshelfIntegral, ksLowshelfTime, pieceTime)
import WAGS.Graph.Constructors (Lowshelf, Gain, PlayBuf, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, lowshelf, playBuf, speaker)
import WAGS.Universe.AudioUnit (TLowshelf, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

ksLowshelfBegins = ksLowshelfIntegral - ksLowshelfTime :: Number

type LowshelfGraph
  = GraphC
      (NodeC (TLowshelf D2) (SingleEdge D3))
      ( NodeListCons
          (NodeC (TPlayBuf D3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D2)) NodeListNil)
          )
      )

type LowshelfUniverse cb
  = Universe' D4 LowshelfGraph cb

type KsLowshelfreate (t :: Type -> Type) b
  = t (Lowshelf GetSetAP GetSetAP (b (PlayBuf "my-buffer" GetSetAP)))

type KsLowshelf g t b
  = Speaker (g (Gain GetSetAP (KsLowshelfreate t b)))

ksLowshelfCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsLowshelfreate t b
ksLowshelfCreate ft fb = ft $ lowshelf { freq: 300.0 } (fb $ playBuf (Proxy :: _ "my-buffer"))

ksLowshelf' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsLowshelf g t b
ksLowshelf' fg ft fb =
  speaker
    (fg $ gain 1.0 (ksLowshelfCreate ft fb))

ksLowshelf :: KsLowshelf Identity Identity Identity
ksLowshelf = ksLowshelf' Identity Identity Identity

ksLowshelfPlaybuf :: KsLowshelf Identity Identity Focus
ksLowshelfPlaybuf = ksLowshelf' Identity Identity Focus

ksLowshelfLowshelf :: KsLowshelf Identity Focus Identity
ksLowshelfLowshelf = ksLowshelf' Identity Focus Identity

ksLowshelfGain :: KsLowshelf Focus Identity Identity
ksLowshelfGain = ksLowshelf' Focus Identity Identity

deltaKsLowshelf :: Number -> KsLowshelf Identity Identity Identity
deltaKsLowshelf =
  (_ % pieceTime)
    >>> (_ - ksLowshelfBegins)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ lowshelf { freq: (calcSlope 0.0 300.0 ksLowshelfTime 200.0 time) } (Identity $ playBuf (Proxy :: _ "my-buffer")))
          )
