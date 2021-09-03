module WAGS.Example.KitchenSink.Types.Notch where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CNotch, CPlayBuf, notch, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TNotch, TPlayBuf)
import WAGS.Math (calcSlope)

type NotchGraph
  = TopWith { notch :: Unit }
  ( notch :: TNotch /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

ksNotchCreate :: World -> { notch :: CNotch { buf :: CPlayBuf } }
ksNotchCreate { buffers: { "my-buffer": myBuffer } } = { notch: notch { freq: 300.0 } { buf: playBuf myBuffer } }

deltaKsNotch :: forall proof. World -> Number -> IxWAGSig' NotchGraph NotchGraph proof Unit
deltaKsNotch { buffers: { "my-buffer": myBuffer, shruti } } =
  (_ % pieceTime)
    >>> (_ - timing.ksNotch.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        switchOO = time % 2.0 < 1.0
        switchW = time % 4.0 < 2.0
        mix = if time > (timing.ksNotch.dur - 1.0) then 0.0 else 1.0
        notch = calcSlope 0.0 300.0 timing.ksNotch.dur 2000.0 time
        onOff = if switchOO then On else Off
        buffer = if switchW then myBuffer else shruti
      in
        ichange { mix, notch, buf: { onOff, buffer } }
