module WAGS.Example.KitchenSink.Types.LoopBuf where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math (pi, sin, (%))
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CLoopBuf, loopBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TLoopBuf)

type LoopBufGraph
  = TopWith { loopBuf :: Unit }
      ( loopBuf :: TLoopBuf /\ {}
      )

ksLoopBufCreate :: { loopBuf :: CLoopBuf "my-buffer" }
ksLoopBufCreate = { loopBuf: loopBuf { playbackRate: 1.0, loopStart: 1.0, loopEnd: 2.5 } (Proxy :: _ "my-buffer") }

deltaKsLoopBuf :: forall proof. Number -> IxWAGSig' LoopBufGraph LoopBufGraph proof Unit
deltaKsLoopBuf =
  (_ % pieceTime)
    >>> (_ - timing.ksLoopBuf.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time

          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0

          changeRec =
            { onOff: if switchOO then On else Off
            , playbackRate: 1.0 + (0.1 * sin rad)
            , loopStart: 1.0
            , loopEnd: 1.4 + 0.2 * (sin rad)
            }
        in
          if switchW then
            ichange
              { loopBuf:
                  Record.union changeRec
                    { buffer: Proxy :: _ "my-buffer"
                    }
              }
          else
            ichange
              { loopBuf:
                  Record.union changeRec
                    { buffer: Proxy :: _ "shruti"
                    }
              }
