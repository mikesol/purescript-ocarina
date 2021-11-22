module WAGS.Example.KitchenSink.Types.StereoPanner where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math (sin, (%), pi)
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CStereoPanner, CPlayBuf, pan, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TPlayBuf, TStereoPanner, _off, _on)

type StereoPannerGraph
  = TopWith { pan :: Unit }
  ( pan :: TStereoPanner /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

ksStereoPannerCreate :: World -> { pan :: CStereoPanner { buf :: CPlayBuf } }
ksStereoPannerCreate { buffers: { "my-buffer": myBuffer } } = { pan: pan 0.0 { buf: playBuf myBuffer } }

deltaKsStereoPanner :: forall proof. World -> Number -> IxWAGSig' StereoPannerGraph StereoPannerGraph proof Unit
deltaKsStereoPanner { buffers: { "my-buffer": myBuffer, shruti } } =
  (_ % pieceTime)
    >>> (_ - timing.ksStereoPanner.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        switchOO = time % 2.0 < 1.0
        switchW = time % 4.0 < 2.0
        mix = if time > (timing.ksStereoPanner.dur - 1.0) then 0.0 else 1.0
        onOff = if switchOO then _on else _off
        buffer = if switchW then myBuffer else shruti
        pan = sin (time * pi)
      in
        ichange { mix, pan, buf: { onOff, buffer } }
