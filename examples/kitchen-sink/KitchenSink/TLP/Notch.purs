module WAGS.Example.KitchenSink.TLP.Notch where

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
import WAGS.Example.KitchenSink.TLP.Peaking (doPeaking)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Notch (NotchUniverse, ksNotchNotch, ksNotchGain, ksNotchPlaybuf, deltaKsNotch)
import WAGS.Example.KitchenSink.Types.Peaking (ksPeakingCreate)

doNotch :: forall proof iu cb. StepSig (NotchUniverse cb) proof iu
doNotch =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksNotchNotch
    toRemoveBuf <- cursor ksNotchPlaybuf
    gn <- cursor ksNotchGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksNotch.end then
          Right (change (deltaKsNotch time) $> lsig)
        else
          Left
            $ inSitu doPeaking WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemove
                destroy toRemoveBuf
                reset
                toAdd <- create (ksPeakingCreate Identity Identity)
                connect toAdd gn
                withProof pr lsig
