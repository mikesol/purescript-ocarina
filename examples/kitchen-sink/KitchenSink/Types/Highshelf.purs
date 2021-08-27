module WAGS.Example.KitchenSink.Types.Highshelf where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CHighshelf, CPlayBuf, highshelf, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), THighshelf, TPlayBuf)
import WAGS.Math (calcSlope)

type HighshelfGraph
  = TopWith { highshelf :: Unit }
      ( highshelf :: THighshelf /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksHighshelfCreate :: { highshelf :: CHighshelf { buf :: CPlayBuf "my-buffer" } }
ksHighshelfCreate = { highshelf: highshelf { freq: 300.0 } { buf: playBuf (Proxy :: _ "my-buffer") } }

deltaKsHighshelf :: forall proof. Number -> IxWAGSig' HighshelfGraph HighshelfGraph proof Unit
deltaKsHighshelf =
  (_ % pieceTime)
    >>> (_ - timing.ksHighshelf.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0

          mix = if time > timing.ksHighshelf.dur - 1.0 then 0.0 else 1.0

          highshelf = calcSlope 0.0 300.0 timing.ksHighshelf.dur 2000.0 time

          onOff = if switchOO then On else Off
        in
          if switchW then
            ichange
              { mix
              , highshelf
              , buf: { onOff, buffer: Proxy :: _ "my-buffer" }
              }
          else
            ichange
              { mix
              , highshelf
              , buf: { onOff, buffer: Proxy :: _ "shruti" }
              }
