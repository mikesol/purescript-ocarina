module WAGS.Example.KitchenSink.Types.Delay where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CDelay, CGain, CPlayBuf, Ref, delay, gain, playBuf, ref)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TDelay, TGain, TPlayBuf)
import WAGS.Graph.Parameter (_off, _on)
import WAGS.Math (calcSlope)

type DelayGraph
  = TopWith { dmix :: Unit }
  ( dmix :: TGain /\ { delay :: Unit, buf :: Unit }
  , delay :: TDelay /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

ksDelayCreate :: World -> { dmix :: CGain { delay :: CDelay { buf :: CPlayBuf }, buf :: Ref } }
ksDelayCreate { buffers: { "my-buffer": myBuffer } } =
  { dmix:
      gain 1.0
        { delay: delay 0.3 { buf: playBuf myBuffer }
        , buf: ref
        }
  }

deltaKsDelay :: forall proof. World -> Number -> IxWAGSig' DelayGraph DelayGraph proof Unit
deltaKsDelay { buffers: { "my-buffer": myBuffer, shruti } } =
  (_ % pieceTime)
    >>> (_ - timing.ksDelay.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        switchOO = time % 2.0 < 1.0
        switchW = time % 4.0 < 2.0
        changes =
          { mix: if time > (timing.ksDelay.dur - 1.0) then 0.0 else 1.0
          , delay: calcSlope 0.0 0.3 timing.ksDelay.dur 0.6 time
          , buf: { onOff: if switchOO then _on else _off, buffer: if switchW then myBuffer else shruti }
          }
      in
        ichange changes
