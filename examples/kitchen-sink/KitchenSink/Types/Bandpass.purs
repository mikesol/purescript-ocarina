module WAGS.Example.KitchenSink.Types.Bandpass where

import Prelude
import Data.Identity (Identity(..))
import Math ((%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, TopLevel)
import WAGS.Graph.Constructors (Bandpass, PlayBuf)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, bandpass, gain, playBuf, speaker)
import WAGS.Universe.AudioUnit (TBandpass, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type BandpassGraph
  = GraphC
      (NodeC (TBandpass EI0) (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1) NoEdge)
          (BaseGraph EI0)
      )

type BandpassUniverse cb
  = Universe' EI2 BandpassGraph cb

type KsBandpassreate (t :: Type -> Type) b
  = t (Bandpass GetSetAP GetSetAP (b (PlayBuf GetSetAP)))

type KsBandpass g t b
  = TopLevel g (KsBandpassreate t b)

ksBandpassCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsBandpassreate t b
ksBandpassCreate ft fb = ft $ bandpass { freq: 300.0 } (fb $ playBuf "my-buffer")

ksBandpass' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsBandpass g t b
ksBandpass' fg ft fb =
  speaker
    (fg $ gain 1.0 (ksBandpassCreate ft fb))

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
    >>> (_ - timing.ksBandpass.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksBandpass.dur - 1.0) then 0.0 else 1.0)
                  (Identity $ bandpass { freq: calcSlope 0.0 300.0 timing.ksBandpass.dur 2000.0 time } (Identity $ playBuf "my-buffer"))
          )
