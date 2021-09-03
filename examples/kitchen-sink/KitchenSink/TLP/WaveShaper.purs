module WAGS.Example.KitchenSink.TLP.WaveShaper where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, icont)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.Delay (doDelay)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Delay (ksDelayCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.WaveShaper (WaveShaperGraph, deltaKsWaveShaper)
import WAGS.Run (SceneI(..))

doWaveShaper :: forall proof. StepSig WaveShaperGraph proof
doWaveShaper =
  ibranch \(SceneI { time, world }) lsig ->
    if time % pieceTime < timing.ksWaveShaper.end then
      Right (deltaKsWaveShaper time $> lsig)
    else
      Left
        $ icont doDelay Ix.do
            let
              cursorWaveShaper = Proxy :: _ "waveShaper"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorWaveShaper }
            idisconnect { source: cursorWaveShaper, dest: cursorGain }
            idestroy cursorPlayBuf
            idestroy cursorWaveShaper
            icreate (ksDelayCreate world)
            iconnect { source: Proxy :: _ "dmix", dest: cursorGain }
            ipure lsig
