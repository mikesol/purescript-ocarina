module WAGS.Example.KitchenSink.TLP.Lowpass where

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
import WAGS.Example.KitchenSink.TLP.Highshelf (doHighshelf)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Highshelf (ksHighshelfCreate)
import WAGS.Example.KitchenSink.Types.Lowpass (LowpassUniverse, ksLowpassLowpass, ksLowpassGain, ksLowpassPlaybuf, deltaKsLowpass)

doLowpass :: forall proof iu cb. StepSig (LowpassUniverse cb) proof iu
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
                cursorLowpass <- cursor ksLowpassLowpass
                cursorPlayBuf <- cursor ksLowpassPlaybuf
                cursorGain <- cursor ksLowpassGain
                disconnect cursorPlayBuf cursorLowpass
                disconnect cursorLowpass cursorGain
                destroy cursorLowpass
                destroy cursorPlayBuf
                reset
                toAdd <- create (ksHighshelfCreate Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
