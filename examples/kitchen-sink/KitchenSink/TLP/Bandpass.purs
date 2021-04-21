module WAGS.Example.KitchenSink.TLP.Bandpass where

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
import WAGS.Example.KitchenSink.TLP.Notch (doNotch)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Bandpass (BandpassUniverse, ksBandpassBandpass, ksBandpassGain, ksBandpassPlaybuf, deltaKsBandpass)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Notch (ksNotchCreate)

doBandpass :: forall proof iu cb. StepSig (BandpassUniverse cb) proof iu
doBandpass =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksBandpassBandpass
    toRemoveBuf <- cursor ksBandpassPlaybuf
    gn <- cursor ksBandpassGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksBandpass.end then
          Right (change (deltaKsBandpass time) $> lsig)
        else
          Left
            $ inSitu doNotch WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemove
                destroy toRemoveBuf
                reset
                toAdd <- create (ksNotchCreate Identity Identity)
                connect toAdd gn
                withProof pr lsig
