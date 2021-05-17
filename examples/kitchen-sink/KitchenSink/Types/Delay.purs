module WAGS.Example.KitchenSink.Types.Delay where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Example.KitchenSink.Timing (calcSlope, timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TDelay, TGain, TPlayBuf)
import WAGS.Graph.Optionals (CDelay, CGain, CPlayBuf, DDelay, DGain, DPlayBuf, Ref, delay, delay_, gain, gain_, playBuf, playBuf_, ref)

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

deltaKsDelay :: Number -> { mix :: DGain, delay :: DDelay, buf :: DPlayBuf }
deltaKsDelay =
  (_ % pieceTime)
    >>> (_ - timing.ksDelay.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksDelay.dur - 1.0) then 0.0 else 1.0)
          , delay: delay_ $ calcSlope 0.0 0.3 timing.ksDelay.dur 0.6 time
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
