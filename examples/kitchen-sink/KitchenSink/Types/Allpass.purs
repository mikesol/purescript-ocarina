module WAGS.Example.KitchenSink.Types.Allpass where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Math (calcSlope)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TAllpass, TPlayBuf)
import WAGS.Graph.Optionals (CAllpass, CPlayBuf, DAllpass, DGain, DPlayBuf, allpass, allpass_, gain_, playBuf, playBuf_)

type AllpassGraph
  = TopWith { allpass :: Unit }
      ( allpass :: TAllpass /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksAllpassCreate :: { allpass :: CAllpass { buf :: CPlayBuf } }
ksAllpassCreate = { allpass: allpass { freq: 300.0 } { buf: playBuf "my-buffer" } }

deltaKsAllpass :: Number -> { mix :: DGain, allpass :: DAllpass, buf :: DPlayBuf }
deltaKsAllpass =
  (_ % pieceTime)
    >>> (_ - timing.ksAllpass.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksAllpass.dur - 1.0) then 0.0 else 1.0)
          , allpass: allpass_ { freq: calcSlope 0.0 300.0 timing.ksAllpass.dur 2000.0 time }
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
