module WAGS.Example.KitchenSink.Types.Lowpass where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CLowpass, CPlayBuf, lowpass, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TLowpass, TPlayBuf, _off, _on)
import WAGS.Math (calcSlope)

type LowpassGraph
  = TopWith { lowpass :: Unit }
  ( lowpass :: TLowpass /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

ksLowpassCreate :: World -> { lowpass :: CLowpass { buf :: CPlayBuf } }
ksLowpassCreate { buffers: { "my-buffer": myBuffer } } = { lowpass: lowpass { freq: 300.0 } { buf: playBuf myBuffer } }

deltaKsLowpass :: forall proof. World -> Number -> IxWAGSig' LowpassGraph LowpassGraph proof Unit
deltaKsLowpass { buffers: { "my-buffer": myBuffer, shruti } } =
  (_ % pieceTime)
    >>> (_ - timing.ksLowpass.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        switchOO = time % 2.0 < 1.0
        switchW = time % 4.0 < 2.0
        mix = if time > (timing.ksLowpass.dur - 1.0) then 0.0 else 1.0
        lowpass = calcSlope 0.0 300.0 timing.ksLowpass.dur 2000.0 time
        onOff = if switchOO then _on else _off
        buffer = if switchW then myBuffer else shruti
      in
        ichange { mix, lowpass, buf: { onOff, buffer } }
