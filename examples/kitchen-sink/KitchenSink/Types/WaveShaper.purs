module WAGS.Example.KitchenSink.Types.WaveShaper where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CPlayBuf, CWaveShaper, playBuf, waveShaper)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), OversampleTwoX(..), TPlayBuf, TWaveShaper)

type WaveShaperGraph
  = TopWith { waveShaper :: Unit }
      ( waveShaper :: TWaveShaper "my-waveshaper" OversampleTwoX /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksWaveShaperCreate :: { waveShaper :: CWaveShaper "my-waveshaper" OversampleTwoX { buf :: CPlayBuf "my-buffer" } }
ksWaveShaperCreate =
  { waveShaper:
      waveShaper (Proxy :: _ "my-waveshaper")
        OversampleTwoX
        { buf: playBuf (Proxy :: _ "my-buffer") }
  }

deltaKsWaveShaper :: forall proof. Number -> IxWAGSig' WaveShaperGraph WaveShaperGraph proof Unit
deltaKsWaveShaper =
  (_ % pieceTime)
    >>> (_ - timing.ksWaveShaper.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0

          mix = if time > (timing.ksWaveShaper.dur - 1.0) then 0.0 else 1.0

          onOff = if switchOO then On else Off
        in
          if switchW then
            ichange
              { mix
              , buf: { onOff, buffer: Proxy :: _ "my-buffer" }
              }
          else
            ichange
              { mix
              , buf: { onOff, buffer: Proxy :: _ "shruti" }
              }
