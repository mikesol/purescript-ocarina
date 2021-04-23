module WAGS.Example.KitchenSink.TLP.WaveShaper where

import Prelude
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.Delay (doDelay)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Delay (ksDelayCreate)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.WaveShaper (WaveShaperUniverse, deltaKsWaveShaper, ksWaveShaperGain, ksWaveShaperPlaybuf, ksWaveShaperWaveShaper)

doWaveShaper :: forall proof iu cb. StepSig (WaveShaperUniverse cb) proof iu
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
                cursorWaveShaper <- cursor ksWaveShaperWaveShaper
                cursorPlayBuf <- cursor ksWaveShaperPlaybuf
                cursorGain <- cursor ksWaveShaperGain
                disconnect cursorPlayBuf cursorWaveShaper
                disconnect cursorWaveShaper cursorGain
                destroy cursorPlayBuf
                destroy cursorWaveShaper
                reset
                toAdd <- create (ksDelayCreate Identity Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
