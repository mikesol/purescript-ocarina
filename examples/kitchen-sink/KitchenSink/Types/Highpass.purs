module WAGS.Example.KitchenSink.Types.Highpass where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Math (calcSlope)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), THighpass, TPlayBuf)
import WAGS.Graph.Optionals (CHighpass, CPlayBuf, DGain, DPlayBuf, DHighpass, highpass, highpass_, gain_, playBuf, playBuf_)

type HighpassGraph
  = TopWith { highpass :: Unit }
      ( highpass :: THighpass /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksHighpassCreate :: { highpass :: CHighpass { buf :: CPlayBuf } }
ksHighpassCreate = { highpass: highpass { freq: 300.0 } { buf: playBuf "my-buffer" } }

deltaKsHighpass :: Number -> { mix :: DGain, highpass :: DHighpass, buf :: DPlayBuf }
deltaKsHighpass =
  (_ % pieceTime)
    >>> (_ - timing.ksHighpass.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksHighpass.dur - 1.0) then 0.0 else 1.0)
          , highpass: highpass_ { freq: calcSlope 0.0 300.0 timing.ksHighpass.dur 2000.0 time }
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
