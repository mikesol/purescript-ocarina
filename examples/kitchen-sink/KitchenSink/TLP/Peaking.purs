module WAGS.Example.KitchenSink.TLP.Peaking where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.Highpass (doHighpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Highpass (ksHighpassCreate)
import WAGS.Example.KitchenSink.Types.Peaking (PeakingUniverse, ksPeakingPeaking, ksPeakingGain, ksPeakingPlaybuf, deltaKsPeaking)

doPeaking :: forall proof iu cb. StepSig (PeakingUniverse cb) proof iu
doPeaking =
  branch \lsig -> WAGS.do
    { time } <- env
    ivoid $ modifyRes (const $ "Using a peaking filter")
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksPeaking.end then
          Right (change (deltaKsPeaking time) $> lsig)
        else
          Left
            $ inSitu doHighpass WAGS.do
                cursorPeaking <- cursor ksPeakingPeaking
                cursorPlayBuf <- cursor ksPeakingPlaybuf
                cursorGain <- cursor ksPeakingGain
                disconnect cursorPlayBuf cursorPeaking
                disconnect cursorPeaking cursorGain
                destroy cursorPeaking
                destroy cursorPlayBuf
                reset
                toAdd <- create (ksHighpassCreate Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
