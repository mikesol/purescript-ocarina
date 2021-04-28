module WAGS.Example.KitchenSink.Types.Highshelf where

import Prelude
import Data.Identity (Identity(..))
import Math ((%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, TopLevel)
import WAGS.Graph.Constructors (Highshelf, PlayBuf)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, highshelf, playBuf, speaker)
import WAGS.Universe.AudioUnit (THighshelf, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type HighshelfGraph
  = GraphC
      (NodeC (THighshelf EI0) (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1) NoEdge)
          (BaseGraph EI0)
      )

type HighshelfUniverse cb
  = Universe' EI2 HighshelfGraph cb

type KsHighshelfreate (t :: Type -> Type) b
  = t (Highshelf GetSetAP GetSetAP (b (PlayBuf GetSetAP)))

type KsHighshelf g t b
  = TopLevel g (KsHighshelfreate t b)

ksHighshelfCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsHighshelfreate t b
ksHighshelfCreate ft fb = ft $ highshelf { freq: 300.0 } (fb $ playBuf "my-buffer")

ksHighshelf' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsHighshelf g t b
ksHighshelf' fg ft fb =
  speaker
    (fg $ gain 1.0 (ksHighshelfCreate ft fb))

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
    >>> (_ - timing.ksHighshelf.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksHighshelf.dur - 1.0) then 0.0 else 1.0)
                  (Identity $ highshelf { freq: calcSlope 0.0 300.0 timing.ksHighshelf.dur 2000.0 time } (Identity $ playBuf "my-buffer"))
          )
