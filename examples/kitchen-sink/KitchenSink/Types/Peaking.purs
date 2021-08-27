module WAGS.Example.KitchenSink.Types.Peaking where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CPeaking, CPlayBuf, peaking, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TPeaking, TPlayBuf)
import WAGS.Math (calcSlope)

type PeakingGraph
  = TopWith { peaking :: Unit }
      ( peaking :: TPeaking /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksPeakingCreate :: { peaking :: CPeaking { buf :: CPlayBuf "my-buffer" } }
ksPeakingCreate = { peaking: peaking { freq: 300.0 } { buf: playBuf (Proxy :: _ "my-buffer") } }

deltaKsPeaking :: forall proof. Number -> IxWAGSig' PeakingGraph PeakingGraph proof Unit
deltaKsPeaking =
  (_ % pieceTime)
    >>> (_ - timing.ksPeaking.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0

          mix = if time > (timing.ksPeaking.dur - 1.0) then 0.0 else 1.0

          peaking = calcSlope 0.0 300.0 timing.ksPeaking.dur 2000.0 time

          onOff = if switchOO then On else Off
        in
          if switchW then
            ichange
              { mix, peaking, buf: { onOff, buffer: Proxy :: _ "my-buffer" }
              }
          else
            ichange
              { mix, peaking, buf: { onOff, buffer: Proxy :: _ "shruti" }
              }
