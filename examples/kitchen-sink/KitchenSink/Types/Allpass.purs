module WAGS.Example.KitchenSink.Types.Allpass where

import Prelude
import Data.Identity (Identity(..))
import Math ((%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, TopLevel)
import WAGS.Graph.Constructors (Allpass, OnOff(..), PlayBuf)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, allpass, gain, playBuf, speaker)
import WAGS.Universe.AudioUnit (TAllpass, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type AllpassGraph
  = GraphC
      (NodeC (TAllpass EI0) (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1) NoEdge)
          (BaseGraph EI0)
      )

type AllpassUniverse cb
  = Universe' EI2 AllpassGraph cb

type KsAllpassCreate (t :: Type -> Type) b
  = t (Allpass GetSetAP GetSetAP (b (PlayBuf GetSetAP)))

type KsAllpass g t b
  = TopLevel g (KsAllpassCreate t b)

ksAllpassCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsAllpassCreate t b
ksAllpassCreate ft fb = ft $ allpass { freq: 300.0 } (fb $ playBuf "my-buffer")

ksAllpass' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsAllpass g t b
ksAllpass' fg ft fb =
  speaker
    (fg $ gain 1.0 (ksAllpassCreate ft fb))

ksAllpass :: KsAllpass Identity Identity Identity
ksAllpass = ksAllpass' Identity Identity Identity

ksAllpassPlaybuf :: KsAllpass Identity Identity Focus
ksAllpassPlaybuf = ksAllpass' Identity Identity Focus

ksAllpassAllpass :: KsAllpass Identity Focus Identity
ksAllpassAllpass = ksAllpass' Identity Focus Identity

ksAllpassGain :: KsAllpass Focus Identity Identity
ksAllpassGain = ksAllpass' Focus Identity Identity

deltaKsAllpass :: Number -> KsAllpass Identity Identity Identity
deltaKsAllpass =
  (_ % pieceTime)
    >>> (_ - timing.ksAllpass.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          speaker
            ( Identity
                $ gain (if time > (timing.ksAllpass.dur - 1.0) then 0.0 else 1.0)
                    ( Identity
                        $ allpass { freq: calcSlope 0.0 300.0 timing.ksAllpass.dur 2000.0 time }
                            ( Identity
                                $ playBuf
                                    { onOff: if switchOO then On else Off
                                    }
                                    (if switchW then "my-buffer" else "shruti")
                            )
                    )
            )
