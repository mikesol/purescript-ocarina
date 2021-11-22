module WAGS.Example.KitchenSink.Types.WaveShaper where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CPlayBuf, CWaveShaper, playBuf, waveShaper)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig', World)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OversampleTwoX(..), TPlayBuf, TWaveShaper, _off, _on)

type WaveShaperGraph
  = TopWith { waveShaper :: Unit }
  ( waveShaper :: TWaveShaper OversampleTwoX /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

ksWaveShaperCreate :: World -> { waveShaper :: CWaveShaper OversampleTwoX { buf :: CPlayBuf } }
ksWaveShaperCreate
  { buffers: { "my-buffer": myBuffer }
  , floatArrays: { "my-waveshaper": myWaveshaper }
  } =
  { waveShaper:
      waveShaper myWaveshaper
        OversampleTwoX
        { buf: playBuf myBuffer }
  }

deltaKsWaveShaper :: forall proof. Number -> IxWAGSig' WaveShaperGraph WaveShaperGraph proof Unit
deltaKsWaveShaper =
  (_ % pieceTime)
    >>> (_ - timing.ksWaveShaper.begin)
    >>> (max 0.0)
    >>> \time ->
      let
        switchOO = time % 2.0 < 1.0
        mix = if time > (timing.ksWaveShaper.dur - 1.0) then 0.0 else 1.0
        onOff = if switchOO then _on else _off
      in
        ichange { mix, buf: { onOff } }
