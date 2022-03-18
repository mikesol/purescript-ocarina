module WAGS.Example.KitchenSink.Types.Bandpass where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CBandpass, CPlayBuf, bandpass, playBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TBandpass, TPlayBuf)
import WAGS.Graph.Parameter (_off, _on)
import WAGS.Math (calcSlope)

type BandpassGraph = TopWith { bandpass :: Unit }
  ( bandpass :: TBandpass /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

ksBandpassCreate :: World -> { bandpass :: CBandpass { buf :: CPlayBuf } }
ksBandpassCreate { buffers: { "my-buffer": myBuffer } } =
  { bandpass: bandpass { freq: 300.0 } { buf: playBuf myBuffer } }

deltaKsBandpass
  :: forall proof
   . World
  -> Number
  -> IxWAGSig' BandpassGraph BandpassGraph proof Unit
deltaKsBandpass { buffers: { "my-buffer": myBuffer, shruti } } =
  (_ % pieceTime)
    >>> (_ - timing.ksBandpass.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        switchOO = time % 2.0 < 1.0
        switchW = time % 4.0 < 2.0
        onOff = if switchOO then _on else _off
        buffer = if switchW then myBuffer else shruti
        changes =
          { mix: if time > (timing.ksBandpass.dur - 1.0) then 0.0 else 1.0
          , bandpass:
              { freq: calcSlope 0.0 300.0 timing.ksBandpass.dur 2000.0 time
              , q: calcSlope 0.0 1.0 timing.ksBandpass.dur 20.0 time
              }
          , buf: { onOff, buffer }
          }
      in
        ichange changes