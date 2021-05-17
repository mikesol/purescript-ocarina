module WAGS.Example.KitchenSink.Types.LoopBuf where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (pi, sin, (%))
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TLoopBuf)
import WAGS.Graph.Optionals (CLoopBuf, DLoopBuf, loopBuf, loopBuf_)

type LoopBufGraph
  = TopWith { loopBuf :: Unit }
      ( loopBuf :: TLoopBuf /\ {}
      )

ksLoopBufCreate :: { loopBuf :: CLoopBuf }
ksLoopBufCreate = { loopBuf: loopBuf { playbackRate: 1.0, start: 1.0, end: 2.5 } "my-buffer" }

deltaKsLoopBuf :: Number -> { loopBuf :: DLoopBuf }
deltaKsLoopBuf =
  (_ % pieceTime)
    >>> (_ - timing.ksLoopBuf.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time

          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { loopBuf:
              loopBuf_
                { onOff: if switchOO then On else Off
                , playbackRate: 1.0 + (0.1 * sin rad)
                , start: 1.0
                , end: 1.4 + 0.2 * (sin rad)
                }
                (if switchW then "my-buffer" else "shruti")
          }
