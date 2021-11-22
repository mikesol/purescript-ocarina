module WAGS.Example.KitchenSink.Types.Feedback where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CDelay, CGain, CPlayBuf, Ref, CHighpass, delay, gain, highpass, playBuf, ref)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TDelay, TGain, THighpass, TPlayBuf, _off, _on)
import WAGS.Math (calcSlope)

type FeedbackGraph
  = TopWith { dmix :: Unit }
  ( dmix :: TGain /\ { delay :: Unit, buf :: Unit }
  , delay :: TDelay /\ { highpass :: Unit }
  , buf :: TPlayBuf /\ {}
  , highpass :: THighpass /\ { dmix :: Unit }
  )

ksFeedbackCreate
  :: World
  -> { dmix ::
         CGain
           { delay :: CDelay { highpass :: CHighpass { dmix :: Ref } }
           , buf :: CPlayBuf
           }
     }
ksFeedbackCreate { buffers: { "my-buffer": myBuffer } } =
  { dmix:
      gain 1.0
        { delay: delay 0.3 { highpass: highpass 2000.0 { dmix: ref } }
        , buf: playBuf myBuffer
        }
  }

deltaKsFeedback :: forall proof. World -> Number -> IxWAGSig' FeedbackGraph FeedbackGraph proof Unit
deltaKsFeedback { buffers: { "my-buffer": myBuffer, shruti } } =
  (_ % pieceTime)
    >>> (_ - timing.ksFeedback.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        switchOO = time % 2.0 < 1.0
        switchW = time % 4.0 < 2.0
        mix = if time > (timing.ksFeedback.dur - 1.0) then 0.0 else 1.0
        delay = calcSlope 0.0 0.3 timing.ksFeedback.dur 0.6 time
        onOff = if switchOO then _on else _off
        buffer = if switchW then myBuffer else shruti
      in
        ichange { mix, delay, buf: { onOff, buffer } }
