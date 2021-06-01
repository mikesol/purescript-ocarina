module WAGS.Example.KitchenSink.TLP.Highpass where

import Prelude
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch)
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.Microphone (doMicrophone)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highpass (HighpassGraph, deltaKsHighpass)
import WAGS.Example.KitchenSink.Types.Microphone (ksMicrophoneCreate)

doHighpass :: forall proof. StepSig HighpassGraph proof
doHighpass =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksHighpass.end then
          Right (change (deltaKsHighpass time) $> lsig)
        else
          Left
            $ inSitu doMicrophone WAGS.do
                let
                  cursorHighpass = Proxy :: _ "highpass"

                  cursorPlayBuf = Proxy :: _ "buf"
                disconnect cursorPlayBuf cursorHighpass
                disconnect cursorHighpass cursorGain
                destroy cursorHighpass
                destroy cursorPlayBuf
                create ksMicrophoneCreate
                connect (Proxy :: _ "microphone") cursorGain
                withProof pr lsig
