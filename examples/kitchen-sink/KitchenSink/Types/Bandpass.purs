module WAGS.Example.KitchenSink.Types.Bandpass where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CBandpass, CPlayBuf, bandpass, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TBandpass, TPlayBuf)
import WAGS.Math (calcSlope)

type BandpassGraph
  = TopWith { bandpass :: Unit }
      ( bandpass :: TBandpass /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksBandpassCreate :: { bandpass :: CBandpass { buf :: CPlayBuf "my-buffer" } }
ksBandpassCreate = { bandpass: bandpass { freq: 300.0 } { buf: playBuf (Proxy :: _ "my-buffer") } }

deltaKsBandpass :: forall proof. Number -> IxWAGSig' BandpassGraph BandpassGraph proof Unit
deltaKsBandpass =
  (_ % pieceTime)
    >>> (_ - timing.ksBandpass.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0

          onOff = if switchOO then On else Off

          changes =
            { mix: if time > (timing.ksBandpass.dur - 1.0) then 0.0 else 1.0
            , bandpass: calcSlope 0.0 300.0 timing.ksBandpass.dur 2000.0 time
            }
        in
          if switchW then
            ichange
              $ Record.union
                  changes
                  { buf: { onOff, buffer: Proxy :: _ "my-buffer" }
                  }
          else
            ichange
              $ Record.union
                  changes
                  { buf: { onOff, buffer: Proxy :: _ "shruti" }
                  }
