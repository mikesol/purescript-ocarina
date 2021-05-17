module WAGS.Example.KitchenSink.TLP.Lowpass where

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
import WAGS.Example.KitchenSink.TLP.Highshelf (doHighshelf)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highshelf (ksHighshelfCreate)
import WAGS.Example.KitchenSink.Types.Lowpass (LowpassGraph, deltaKsLowpass)

doLowpass :: forall proof iu. StepSig LowpassGraph proof { | iu }
doLowpass =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksLowpass.end then
          Right (change (deltaKsLowpass time) $> lsig)
        else
          Left
            $ inSitu doHighshelf WAGS.do
                let
                  cursorLowpass = Proxy :: _ "lowpass"

                  cursorPlayBuf = Proxy :: _ "buf"
                disconnect cursorPlayBuf cursorLowpass
                disconnect cursorLowpass cursorGain
                destroy cursorLowpass
                destroy cursorPlayBuf
                create ksHighshelfCreate
                connect (Proxy :: _ "highshelf") cursorGain
                withProof pr lsig
