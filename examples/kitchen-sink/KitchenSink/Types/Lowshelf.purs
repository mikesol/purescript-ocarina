module WAGS.Example.KitchenSink.Types.Lowshelf where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Math (calcSlope)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TLowshelf, TPlayBuf)
import WAGS.Create.Optionals (CLowshelf, CPlayBuf, lowshelf, playBuf)

type LowshelfGraph
  = TopWith { lowshelf :: Unit }
      ( lowshelf :: TLowshelf /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksLowshelfCreate :: { lowshelf :: CLowshelf { buf :: CPlayBuf } }
ksLowshelfCreate = { lowshelf: lowshelf { freq: 300.0 } { buf: playBuf "my-buffer" } }

deltaKsLowshelf :: Number -> ?hole --{ mix :: DGain, lowshelf :: DLowshelf, buf :: DPlayBuf }
deltaKsLowshelf =
  (_ % pieceTime)
    >>> (_ - timing.ksLowshelf.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksLowshelf.dur - 1.0) then 0.0 else 1.0)
          , lowshelf: lowshelf_ { freq: calcSlope 0.0 300.0 timing.ksLowshelf.dur 2000.0 time }
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
