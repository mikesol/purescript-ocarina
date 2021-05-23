module WAGS.Example.KitchenSink.TLP.LoopBuf where

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
import WAGS.Example.KitchenSink.TLP.StereoPanner (doStereoPanner)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.LoopBuf (LoopBufGraph, deltaKsLoopBuf)
import WAGS.Example.KitchenSink.Types.StereoPanner (ksStereoPannerCreate)

doLoopBuf :: forall proof. StepSig LoopBufGraph proof
doLoopBuf =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksLoopBuf.end then
          Right (change (deltaKsLoopBuf time) $> lsig)
        else
          Left
            $ inSitu doStereoPanner WAGS.do
                let
                  cursorLoopBuf = Proxy :: _ "loopBuf"
                disconnect cursorLoopBuf cursorGain
                destroy cursorLoopBuf
                create ksStereoPannerCreate
                connect (Proxy :: _ "pan") cursorGain
                withProof pr lsig
