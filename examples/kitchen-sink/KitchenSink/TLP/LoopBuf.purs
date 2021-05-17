module WAGS.Example.KitchenSink.TLP.LoopBuf where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Math ((%))
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
import WAGS.Example.KitchenSink.Types.LoopBuf (LoopBufUniverse, deltaKsLoopBuf, ksLoopBufGain, ksLoopBufLoopBuf)
import WAGS.Example.KitchenSink.Types.StereoPanner (ksStereoPannerCreate)

doLoopBuf :: forall proof iu cb. StepSig (LoopBufUniverse cb) proof iu
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
                cursorLoopBuf <- cursor ksLoopBufLoopBuf
                cursorGain <- cursor ksLoopBufGain
                disconnect cursorLoopBuf cursorGain
                destroy cursorLoopBuf
                reset
                toAdd <- create (ksStereoPannerCreate Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
