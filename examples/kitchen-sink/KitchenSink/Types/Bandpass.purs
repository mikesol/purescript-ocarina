module WAGS.Example.KitchenSink.Types.Bandpass where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Example.KitchenSink.Timing (calcSlope, timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TBandpass, TPlayBuf)
import WAGS.Graph.Optionals (CBandpass, CPlayBuf, DGain, DPlayBuf, DBandpass, bandpass, bandpass_, gain_, playBuf, playBuf_)

type BandpassGraph
  = TopWith { bandpass :: Unit }
      ( bandpass :: TBandpass /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksBandpassCreate :: { bandpass :: CBandpass { buf :: CPlayBuf } }
ksBandpassCreate = { bandpass: bandpass { freq: 300.0 } { buf: playBuf "my-buffer" } }

deltaKsBandpass :: Number -> { mix :: DGain, bandpass :: DBandpass, buf :: DPlayBuf }
deltaKsBandpass =
  (_ % pieceTime)
    >>> (_ - timing.ksBandpass.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksBandpass.dur - 1.0) then 0.0 else 1.0)
          , bandpass: bandpass_ { freq: calcSlope 0.0 300.0 timing.ksBandpass.dur 2000.0 time }
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
