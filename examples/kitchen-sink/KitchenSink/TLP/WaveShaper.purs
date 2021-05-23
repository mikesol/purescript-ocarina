module WAGS.Example.KitchenSink.TLP.WaveShaper where

import Prelude
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.Delay (doDelay)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Delay (ksDelayCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.WaveShaper (WaveShaperGraph, deltaKsWaveShaper)

doWaveShaper :: forall proof. StepSig WaveShaperGraph proof
doWaveShaper =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksWaveShaper.end then
          Right (change (deltaKsWaveShaper time) $> lsig)
        else
          Left
            $ inSitu doDelay WAGS.do
                let
                  cursorWaveShaper = Proxy :: _ "waveShaper"

                  cursorPlayBuf = Proxy :: _ "buf"
                disconnect cursorPlayBuf cursorWaveShaper
                disconnect cursorWaveShaper cursorGain
                destroy cursorPlayBuf
                destroy cursorWaveShaper
                create ksDelayCreate
                connect (Proxy :: _ "dmix") cursorGain
                withProof pr lsig
