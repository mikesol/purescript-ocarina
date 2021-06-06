module WAGS.Example.KitchenSink.Types.Delay where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CDelay, CGain, CPlayBuf, Ref, delay, gain, playBuf, ref)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TDelay, TGain, TPlayBuf)
import WAGS.Math (calcSlope)

type DelayGraph
  = TopWith { dmix :: Unit }
      ( dmix :: TGain /\ { delay :: Unit, buf :: Unit }
      , delay :: TDelay /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksDelayCreate :: { dmix :: CGain { delay :: CDelay { buf :: CPlayBuf }, buf :: Ref } }
ksDelayCreate =
  { dmix:
      gain 1.0
        { delay: delay 0.3 { buf: playBuf "my-buffer" }
        , buf: ref
        }
  }

deltaKsDelay :: forall proof. Number -> IxWAGSig' DelayGraph DelayGraph proof Unit
deltaKsDelay =
  (_ % pieceTime)
    >>> (_ - timing.ksDelay.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          ichange
            { mix: if time > (timing.ksDelay.dur - 1.0) then 0.0 else 1.0
            , delay: calcSlope 0.0 0.3 timing.ksDelay.dur 0.6 time
            , buf: { onOff: if switchOO then On else Off, buffer: if switchW then "my-buffer" else "shruti" }
            }
