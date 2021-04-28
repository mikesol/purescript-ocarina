module WAGS.Example.KitchenSink.Types.Delay where

import Prelude
import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\), (/\))
import Math ((%))
import Type.Proxy (Proxy)
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, EI3, TopLevel)
import WAGS.Graph.Constructors (Delay, Dup, PlayBuf)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, Mix, delay, dup, gain, mix, playBuf, speaker)
import WAGS.Universe.AudioUnit (TDelay, TGain, TPlayBuf)
import WAGS.Universe.Bin (PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type DelayGraph
  = GraphC
      (NodeC (TGain EI1) (ManyEdges EI2 (PtrListCons EI0 PtrListNil)))
      ( NodeListCons
          (NodeC (TDelay EI2) (SingleEdge EI0))
          ( NodeListCons
              (NodeC (TPlayBuf EI0) NoEdge)
              (BaseGraph EI1)
          )
      )

type DelayUniverse cb
  = Universe' EI3 DelayGraph cb

data MyPlayBuf

type KsDelayCreate (t :: Type -> Type) b mx
  = Dup (b (PlayBuf GetSetAP))
      ( Proxy MyPlayBuf ->
        mx
          ( Mix
              ( (t (Delay GetSetAP (Proxy MyPlayBuf)))
                  /\ Proxy MyPlayBuf
                  /\ Unit
              )
          )
      )

type KsDelay g t b mx
  = TopLevel g (KsDelayCreate t b mx)

ksDelayCreate ::
  forall t b mx.
  Decorating' t ->
  Decorating' b ->
  Decorating' mx ->
  KsDelayCreate t b mx
ksDelayCreate ft fb fmx =
  dup
    (fb $ playBuf "my-buffer")
    (\(myPlayBuf :: Proxy MyPlayBuf) -> fmx $ mix ((ft $ delay 1.0 myPlayBuf) /\ myPlayBuf /\ unit))

ksDelay' ::
  forall g t b mx.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  Decorating' mx ->
  KsDelay g t b mx
ksDelay' fg ft fb fmx = speaker (fg $ gain 1.0 (ksDelayCreate ft fb fmx))

ksDelay :: KsDelay Identity Identity Identity Identity
ksDelay = ksDelay' Identity Identity Identity Identity

ksDelayPlaybuf :: KsDelay Identity Identity Focus Identity
ksDelayPlaybuf = ksDelay' Identity Identity Focus Identity

ksDelayDelay :: KsDelay Identity Focus Identity Identity
ksDelayDelay = ksDelay' Identity Focus Identity Identity

ksDelayGain :: KsDelay Focus Identity Identity Identity
ksDelayGain = ksDelay' Focus Identity Identity Identity

ksDelayMix :: KsDelay Identity Identity Identity Focus
ksDelayMix = ksDelay' Identity Identity Identity Focus

deltaKsDelay :: Number -> KsDelay Identity Identity Identity Identity
deltaKsDelay =
  (_ % pieceTime)
    >>> (_ - timing.ksDelay.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksDelay.dur - 1.0) then 0.0 else 1.0)
                  (ksDelayCreate Identity Identity Identity)
          )
