module WAGS.Example.KitchenSink.TLP.Microphone where

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
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.WaveShaper (doWaveShaper)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Microphone (MicrophoneUniverse, ksMicrophoneMicrophone, ksMicrophoneGain, deltaKsMicrophone)
import WAGS.Example.KitchenSink.Types.WaveShaper (ksWaveShaperCreate)


doMicrophone :: forall proof iu cb. StepSig (MicrophoneUniverse cb) proof iu
doMicrophone =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksMicrophone.end then
          Right (change (deltaKsMicrophone time) $> lsig)
        else
          Left
            $ inSitu doWaveShaper WAGS.do
                cursorMicrophone <- cursor ksMicrophoneMicrophone
                cursorGain <- cursor ksMicrophoneGain
                disconnect cursorMicrophone cursorGain
                destroy cursorMicrophone
                reset
                toAdd <- create (ksWaveShaperCreate Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
