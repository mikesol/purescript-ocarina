module WAGS.Example.KitchenSink.TLP.Allpass where

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
import WAGS.Example.KitchenSink.TLP.Lowpass (doLowpass)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (AllpassUniverse, deltaKsAllpass, ksAllpassAllpass, ksAllpassGain, ksAllpassPlaybuf)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Lowpass (ksLowpassCreate)

doAllpass :: forall proof iu cb. StepSig (AllpassUniverse cb) proof iu
doAllpass =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksAllpassAllpass
    toRemoveBuf <- cursor ksAllpassPlaybuf
    gn <- cursor ksAllpassGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksAllpass.end then
          Right (change (deltaKsAllpass time) $> lsig)
        else
          Left
            $ inSitu doLowpass WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemove
                destroy toRemoveBuf
                reset
                toAdd <- create (ksLowpassCreate Identity Identity)
                connect toAdd gn
                withProof pr lsig
