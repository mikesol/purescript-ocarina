module WAGS.Example.KitchenSink.Types.Lowpass where

import Prelude
import Data.Identity (Identity(..))
import Math ((%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, TopLevel)
import WAGS.Graph.Constructors (Lowpass, PlayBuf)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, lowpass, playBuf, speaker)
import WAGS.Universe.AudioUnit (TLowpass, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type LowpassGraph
  = GraphC
      (NodeC (TLowpass EI0) (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1) NoEdge)
          (BaseGraph EI0)
      )

type LowpassUniverse cb
  = Universe' EI2 LowpassGraph cb

type KsLowpassreate (t :: Type -> Type) b
  = t (Lowpass GetSetAP GetSetAP (b (PlayBuf GetSetAP)))

type KsLowpass g t b
  = TopLevel g (KsLowpassreate t b)

ksLowpassCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsLowpassreate t b
ksLowpassCreate ft fb = ft $ lowpass { freq: 300.0 } (fb $ playBuf "my-buffer")

ksLowpass' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsLowpass g t b
ksLowpass' fg ft fb =
  speaker
    (fg $ gain 1.0 (ksLowpassCreate ft fb))

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
    >>> (_ - timing.ksLowpass.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksLowpass.dur - 1.0) then 0.0 else 1.0)
                  ( Identity
                      $ lowpass { freq: calcSlope 0.0 300.0 timing.ksLowpass.dur 2000.0 time }
                          (Identity $ playBuf "my-buffer")
                  )
          )
