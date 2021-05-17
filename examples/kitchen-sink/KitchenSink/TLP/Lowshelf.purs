module WAGS.Example.KitchenSink.TLP.Lowshelf where

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
import WAGS.Example.KitchenSink.TLP.Bandpass (doBandpass)
import WAGS.Example.KitchenSink.TLP.LoopSig ( StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Bandpass (ksBandpassCreate)
import WAGS.Example.KitchenSink.Types.Lowshelf (LowshelfUniverse, ksLowshelfLowshelf, ksLowshelfGain, ksLowshelfPlaybuf, deltaKsLowshelf)

doLowshelf :: forall proof iu cb. StepSig (LowshelfUniverse cb) proof iu
doLowshelf =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksLowshelf.end then
          Right (change (deltaKsLowshelf time) $> lsig)
        else
          Left
            $ inSitu doBandpass WAGS.do
                cursorLowshelf <- cursor ksLowshelfLowshelf
                cursorPlayBuf <- cursor ksLowshelfPlaybuf
                cursorGain <- cursor ksLowshelfGain
                disconnect cursorPlayBuf cursorLowshelf
                disconnect cursorLowshelf cursorGain
                destroy cursorLowshelf
                destroy cursorPlayBuf
                reset
                toAdd <- create (ksBandpassCreate Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
