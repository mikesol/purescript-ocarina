module WAGS.Example.KitchenSink.Types.LoopBuf where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (pi, sin, (%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CLoopBuf, loopBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TLoopBuf)
import WAGS.Graph.Parameter (_off, _on)

type LoopBufGraph
  = TopWith { loopBuf :: Unit }
  ( loopBuf :: TLoopBuf /\ {}
  )

ksLoopBufCreate :: World -> { loopBuf :: CLoopBuf }
ksLoopBufCreate { buffers: { "my-buffer": myBuffer } } = { loopBuf: loopBuf { playbackRate: 1.0, loopStart: 1.0, loopEnd: 2.5 } myBuffer }

deltaKsLoopBuf :: forall proof. World -> Number -> IxWAGSig' LoopBufGraph LoopBufGraph proof Unit
deltaKsLoopBuf { buffers: { "my-buffer": myBuffer, shruti } } =
  (_ % pieceTime)
    >>> (_ - timing.ksLoopBuf.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        rad = pi * time
        switchOO = time % 2.0 < 1.0
        switchW = time % 4.0 < 2.0
        changeRec =
          { onOff: if switchOO then _on else _off
          , playbackRate: 1.0 + (0.1 * sin rad)
          , loopStart: 1.0
          , loopEnd: 1.4 + 0.2 * (sin rad)
          , buffer: if switchW then myBuffer else shruti
          }
      in
        ichange { loopBuf: changeRec }
