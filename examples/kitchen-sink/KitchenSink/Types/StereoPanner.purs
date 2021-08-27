module WAGS.Example.KitchenSink.Types.StereoPanner where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (sin, (%), pi)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CStereoPanner, CPlayBuf, pan, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TStereoPanner, TPlayBuf)

type StereoPannerGraph
  = TopWith { pan :: Unit }
      ( pan :: TStereoPanner /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksStereoPannerCreate :: { pan :: CStereoPanner { buf :: CPlayBuf "my-buffer" } }
ksStereoPannerCreate = { pan: pan 0.0 { buf: playBuf (Proxy :: _ "my-buffer" )} }

deltaKsStereoPanner :: forall proof. Number -> IxWAGSig' StereoPannerGraph StereoPannerGraph proof Unit
deltaKsStereoPanner =
  (_ % pieceTime)
    >>> (_ - timing.ksStereoPanner.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0

          mix = if time > (timing.ksStereoPanner.dur - 1.0) then 0.0 else 1.0

          onOff = if switchOO then On else Off

          pan = sin (time * pi)
        in
          if switchW then
            ichange
              { mix
              , pan
              , buf: { onOff, buffer: Proxy :: _ "my-buffer" }
              }
          else
            ichange
              { mix
              , pan
              , buf: { onOff, buffer: Proxy :: _ "shruti" }
              }
