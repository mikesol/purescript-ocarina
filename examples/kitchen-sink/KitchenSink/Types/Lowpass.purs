module WAGS.Example.KitchenSink.Types.Lowpass where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Math (calcSlope)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TLowpass, TPlayBuf)
import WAGS.Graph.Optionals (CLowpass, CPlayBuf, DGain, DPlayBuf, DLowpass, lowpass, lowpass_, gain_, playBuf, playBuf_)

type LowpassGraph
  = TopWith { lowpass :: Unit }
      ( lowpass :: TLowpass /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksLowpassCreate :: { lowpass :: CLowpass { buf :: CPlayBuf } }
ksLowpassCreate = { lowpass: lowpass { freq: 300.0 } { buf: playBuf "my-buffer" } }

deltaKsLowpass :: Number -> { mix :: DGain, lowpass :: DLowpass, buf :: DPlayBuf }
deltaKsLowpass =
  (_ % pieceTime)
    >>> (_ - timing.ksLowpass.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksLowpass.dur - 1.0) then 0.0 else 1.0)
          , lowpass: lowpass_ { freq: calcSlope 0.0 300.0 timing.ksLowpass.dur 2000.0 time }
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
