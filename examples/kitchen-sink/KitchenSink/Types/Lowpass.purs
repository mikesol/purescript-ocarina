module WAGS.Example.KitchenSink.Types.Lowpass where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CLowpass, CPlayBuf, lowpass, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TLowpass, TPlayBuf)
import WAGS.Math (calcSlope)

type LowpassGraph
  = TopWith { lowpass :: Unit }
      ( lowpass :: TLowpass /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksLowpassCreate :: { lowpass :: CLowpass { buf :: CPlayBuf "my-buffer" } }
ksLowpassCreate = { lowpass: lowpass { freq: 300.0 } { buf: playBuf (Proxy :: _ "my-buffer") } }

deltaKsLowpass :: forall proof. Number -> IxWAGSig' LowpassGraph LowpassGraph proof Unit
deltaKsLowpass =
  (_ % pieceTime)
    >>> (_ - timing.ksLowpass.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        
          mix = if time > (timing.ksLowpass.dur - 1.0) then 0.0 else 1.0

          lowpass = calcSlope 0.0 300.0 timing.ksLowpass.dur 2000.0 time

          onOff = if switchOO then On else Off
        in
          if switchW then
            ichange
              { mix, lowpass, buf: { onOff, buffer: Proxy :: _ "my-buffer" }
              }
          else
            ichange
              { mix, lowpass, buf: { onOff, buffer: Proxy :: _ "shruti" }
              }