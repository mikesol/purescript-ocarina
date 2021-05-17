module WAGS.Example.KitchenSink.Types.WaveShaper where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), OversampleTwoX(..), TPlayBuf, TWaveShaper)
import WAGS.Graph.Optionals (CPlayBuf, CWaveShaper, DGain, DPlayBuf, gain_, playBuf, playBuf_, waveShaper)

type WaveShaperGraph
  = TopWith { waveShaper :: Unit }
      ( waveShaper :: TWaveShaper "my-waveshaper" OversampleTwoX /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksWaveShaperCreate :: { waveShaper :: CWaveShaper "my-waveshaper" OversampleTwoX { buf :: CPlayBuf } }
ksWaveShaperCreate =
  { waveShaper:
      waveShaper (Proxy :: _ "my-waveshaper")
        OversampleTwoX
        { buf: playBuf "my-buffer" }
  }

deltaKsWaveShaper :: Number -> { mix :: DGain, buf :: DPlayBuf }
deltaKsWaveShaper =
  (_ % pieceTime)
    >>> (_ - timing.ksWaveShaper.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksWaveShaper.dur - 1.0) then 0.0 else 1.0)
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
