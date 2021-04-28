module WAGS.Example.KitchenSink.Types.StereoPanner where

import Prelude
import Data.Identity (Identity(..))
import Math (sin, (%), pi)
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, TopLevel)
import WAGS.Graph.Constructors (StereoPanner, PlayBuf)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, pan, gain, playBuf, speaker)
import WAGS.Universe.AudioUnit (TStereoPanner, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type StereoPannerGraph
  = GraphC
      (NodeC (TStereoPanner EI0) (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1) NoEdge)
          (BaseGraph EI0)
      )

type StereoPannerUniverse cb
  = Universe' EI2 StereoPannerGraph cb

type KsStereoPannerCreate (t :: Type -> Type) b
  = t (StereoPanner GetSetAP (b (PlayBuf GetSetAP)))

type KsStereoPanner g t b
  = TopLevel g (KsStereoPannerCreate t b)

ksStereoPannerCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsStereoPannerCreate t b
ksStereoPannerCreate ft fb = ft $ pan 0.0 (fb $ playBuf "my-buffer")

ksStereoPanner' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsStereoPanner g t b
ksStereoPanner' fg ft fb =
  speaker
    (fg $ gain 1.0 (ksStereoPannerCreate ft fb))

ksStereoPanner :: KsStereoPanner Identity Identity Identity
ksStereoPanner = ksStereoPanner' Identity Identity Identity

ksStereoPannerPlaybuf :: KsStereoPanner Identity Identity Focus
ksStereoPannerPlaybuf = ksStereoPanner' Identity Identity Focus

ksStereoPannerStereoPanner :: KsStereoPanner Identity Focus Identity
ksStereoPannerStereoPanner = ksStereoPanner' Identity Focus Identity

ksStereoPannerGain :: KsStereoPanner Focus Identity Identity
ksStereoPannerGain = ksStereoPanner' Focus Identity Identity

deltaKsStereoPanner :: Number -> KsStereoPanner Identity Identity Identity
deltaKsStereoPanner =
  (_ % pieceTime)
    >>> (_ - timing.ksStereoPanner.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksStereoPanner.dur - 1.0) then 0.0 else 1.0)
                  (Identity $ pan (sin (time * pi)) (Identity $ playBuf "my-buffer"))
          )
