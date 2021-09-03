module WAGS.Example.KitchenSink.Types.Lowshelf where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CLowshelf, CPlayBuf, lowshelf, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TLowshelf, TPlayBuf)
import WAGS.Math (calcSlope)

type LowshelfGraph
  = TopWith { lowshelf :: Unit }
  ( lowshelf :: TLowshelf /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

ksLowshelfCreate :: World -> { lowshelf :: CLowshelf { buf :: CPlayBuf } }
ksLowshelfCreate { buffers: { "my-buffer": myBuffer } } = { lowshelf: lowshelf { freq: 300.0 } { buf: playBuf myBuffer } }

deltaKsLowshelf :: forall proof. World -> Number -> IxWAGSig' LowshelfGraph LowshelfGraph proof Unit
deltaKsLowshelf { buffers: { "my-buffer": myBuffer, shruti } } =
  (_ % pieceTime)
    >>> (_ - timing.ksLowshelf.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        switchOO = time % 2.0 < 1.0
        switchW = time % 4.0 < 2.0
        mix = if time > (timing.ksLowshelf.dur - 1.0) then 0.0 else 1.0
        lowshelf = calcSlope 0.0 300.0 timing.ksLowshelf.dur 2000.0 time
        onOff = if switchOO then On else Off
        buffer = if switchW then myBuffer else shruti
      in
        ichange { mix, lowshelf, buf: { onOff, buffer } }
