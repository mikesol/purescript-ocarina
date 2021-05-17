module WAGS.Example.KitchenSink.Types.Highshelf where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Example.KitchenSink.Timing (calcSlope, timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), THighshelf, TPlayBuf)
import WAGS.Graph.Optionals (CHighshelf, CPlayBuf, DGain, DPlayBuf, DHighshelf, highshelf, highshelf_, gain_, playBuf, playBuf_)

type HighshelfGraph
  = TopWith { highshelf :: Unit }
      ( highshelf :: THighshelf /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksHighshelfCreate :: { highshelf :: CHighshelf { buf :: CPlayBuf } }
ksHighshelfCreate = { highshelf: highshelf { freq: 300.0 } { buf: playBuf "my-buffer" } }

deltaKsHighshelf :: Number -> { mix :: DGain, highshelf :: DHighshelf, buf :: DPlayBuf }
deltaKsHighshelf =
  (_ % pieceTime)
    >>> (_ - timing.ksHighshelf.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksHighshelf.dur - 1.0) then 0.0 else 1.0)
          , highshelf: highshelf_ { freq: calcSlope 0.0 300.0 timing.ksHighshelf.dur 2000.0 time }
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
