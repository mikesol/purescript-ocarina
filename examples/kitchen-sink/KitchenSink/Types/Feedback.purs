module WAGS.Example.KitchenSink.Types.Feedback where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Example.KitchenSink.Timing (calcSlope, timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TDelay, TGain, THighpass, TPlayBuf)
import WAGS.Graph.Optionals (CDelay, CGain, CPlayBuf, DDelay, DGain, DPlayBuf, Ref, CHighpass, delay, delay_, gain, gain_, highpass, playBuf, playBuf_, ref)

type FeedbackGraph
  = TopWith { dmix :: Unit }
      ( dmix :: TGain /\ { delay :: Unit, buf :: Unit }
      , delay :: TDelay /\ { highpass :: Unit }
      , buf :: TPlayBuf /\ {}
      , highpass :: THighpass /\ { dmix :: Unit }
      )

ksFeedbackCreate ::
  { dmix ::
      CGain
        { delay :: CDelay { highpass :: CHighpass { dmix :: Ref } }
        , buf :: CPlayBuf
        }
  }
ksFeedbackCreate =
  { dmix:
      gain 1.0
        { delay: delay 0.3 { highpass: highpass 2000.0 { dmix: ref } }
        , buf: playBuf "my-buffer"
        }
  }

deltaKsFeedback :: Number -> { mix :: DGain, delay :: DDelay, buf :: DPlayBuf }
deltaKsFeedback =
  (_ % pieceTime)
    >>> (_ - timing.ksFeedback.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksFeedback.dur - 1.0) then 0.0 else 1.0)
          , delay: delay_ $ calcSlope 0.0 0.3 timing.ksFeedback.dur 0.6 time
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
