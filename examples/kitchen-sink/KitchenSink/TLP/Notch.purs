module WAGS.Example.KitchenSink.TLP.Notch where

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
import WAGS.Example.KitchenSink.TLP.Peaking (doPeaking)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Notch (NotchGraph, deltaKsNotch)
import WAGS.Example.KitchenSink.Types.Peaking (ksPeakingCreate)

doNotch :: forall proof iu . StepSig NotchGraph proof {|iu}
doNotch =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksNotch.end then
          Right (change (deltaKsNotch time) $> lsig)
        else
          Left
            $ inSitu doPeaking WAGS.do
                let
                  cursorNotch = Proxy :: _ "notch"

                  cursorPlayBuf = Proxy :: _ "buf"
                disconnect cursorPlayBuf cursorNotch
                disconnect cursorNotch cursorGain
                destroy cursorNotch
                destroy cursorPlayBuf
                create ksPeakingCreate 
                connect (Proxy :: _ "peaking") cursorGain
                withProof pr lsig
