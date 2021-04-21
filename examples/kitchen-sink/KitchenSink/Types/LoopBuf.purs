module WAGS.Example.KitchenSink.Types.LoopBuf where

import Prelude
import Data.Identity (Identity(..))
import Math (pi, sin, (%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1)
import WAGS.Graph.Constructors (Gain, Speaker, LoopBuf)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, loopBuf, speaker)
import WAGS.Universe.AudioUnit (TLoopBuf)
import WAGS.Universe.EdgeProfile (NoEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC)

type LoopBufGraph
  = GraphC
      (NodeC (TLoopBuf EI0 "my-buffer") NoEdge)
      (BaseGraph EI0)

type LoopBufUniverse cb
  = Universe' EI1 LoopBufGraph cb

type KsLoopBuf g t
  = Speaker (g (Gain GetSetAP (t (LoopBuf "my-buffer" GetSetAP))))

ksLoopBuf' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  KsLoopBuf g t
ksLoopBuf' fg ft = speaker (fg $ gain 0.0 (ft $ loopBuf { playbackRate: 1.0, start: 1.0, end: 2.5 } (Proxy :: _ "my-buffer")))

ksLoopBuf :: KsLoopBuf Identity Identity
ksLoopBuf = ksLoopBuf' Identity Identity

ksLoopBufLoopBuf :: KsLoopBuf Identity Focus
ksLoopBufLoopBuf = ksLoopBuf' Identity Focus

ksLoopBufGain :: KsLoopBuf Focus Identity
ksLoopBufGain = ksLoopBuf' Focus Identity

deltaKsLoopBuf :: Number -> Speaker (Gain GetSetAP (LoopBuf "my-buffer" GetSetAP))
deltaKsLoopBuf =
  (_ % pieceTime)
    >>> (_ - timing.ksLoopBuf.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          speaker
            $ gain 1.0
                (loopBuf { playbackRate: 1.0 + (0.1 * sin rad), start: 1.0, end: 2.5 + (sin rad) } (Proxy :: _ "my-buffer"))
