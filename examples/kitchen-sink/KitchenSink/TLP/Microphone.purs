module WAGS.Example.KitchenSink.TLP.Microphone where

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
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.WaveShaper (doWaveShaper)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Microphone (MicrophoneGraph, deltaKsMicrophone)
import WAGS.Example.KitchenSink.Types.WaveShaper (ksWaveShaperCreate)

doMicrophone :: forall proof. StepSig MicrophoneGraph proof
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
                let
                  cursorMicrophone = Proxy :: _ "microphone"
                disconnect cursorMicrophone cursorGain
                destroy cursorMicrophone
                create ksWaveShaperCreate
                connect (Proxy :: _ "waveShaper") cursorGain
                withProof pr lsig
