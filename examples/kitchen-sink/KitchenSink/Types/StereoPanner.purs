module WAGS.Example.KitchenSink.Types.StereoPanner where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (sin, (%), pi)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TStereoPanner, TPlayBuf)
import WAGS.Graph.Optionals (CStereoPanner, CPlayBuf, DGain, DPlayBuf, DStereoPanner, pan, pan_, gain_, playBuf, playBuf_)

type StereoPannerGraph
  = TopWith { pan :: Unit }
      ( pan :: TStereoPanner /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksStereoPannerCreate :: { pan :: CStereoPanner { buf :: CPlayBuf } }
ksStereoPannerCreate = { pan: pan 0.0 { buf: playBuf "my-buffer" } }

deltaKsStereoPanner :: Number -> { mix :: DGain, pan :: DStereoPanner, buf :: DPlayBuf }
deltaKsStereoPanner =
  (_ % pieceTime)
    >>> (_ - timing.ksStereoPanner.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksStereoPanner.dur - 1.0) then 0.0 else 1.0)
          , pan: pan_ (sin (time * pi))
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
