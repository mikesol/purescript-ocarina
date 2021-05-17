module WAGS.Example.KitchenSink.TLP.Allpass where

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
import WAGS.Example.KitchenSink.TLP.Lowpass (doLowpass)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (AllpassGraph, deltaKsAllpass)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Lowpass (ksLowpassCreate)

doAllpass :: forall proof iu. StepSig AllpassGraph proof {|iu}
doAllpass =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksAllpass.end then
          Right (change (deltaKsAllpass time) $> lsig)
        else
          Left
            $ inSitu doLowpass WAGS.do
                let
                  cursorAllpass = Proxy :: _ "allpass"

                  cursorPlayBuf = Proxy :: _ "buf"
                disconnect cursorPlayBuf cursorAllpass
                disconnect cursorAllpass cursorGain
                destroy cursorAllpass
                destroy cursorPlayBuf
                create ksLowpassCreate
                connect (Proxy :: _ "lowpass") cursorGain
                withProof pr lsig
