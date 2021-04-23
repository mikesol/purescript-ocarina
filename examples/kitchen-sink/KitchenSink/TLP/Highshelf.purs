module WAGS.Example.KitchenSink.TLP.Highshelf where

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
import WAGS.Example.KitchenSink.TLP.Lowshelf (doLowshelf)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Highshelf (HighshelfUniverse, ksHighshelfHighshelf, ksHighshelfGain, ksHighshelfPlaybuf, deltaKsHighshelf)
import WAGS.Example.KitchenSink.Types.Lowshelf (ksLowshelfCreate)


doHighshelf :: forall proof iu cb. StepSig (HighshelfUniverse cb) proof iu
doHighshelf =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksHighshelf.end then
          Right (change (deltaKsHighshelf time) $> lsig)
        else
          Left
            $ inSitu doLowshelf WAGS.do
                cursorHighshelf <- cursor ksHighshelfHighshelf
                cursorPlayBuf <- cursor ksHighshelfPlaybuf
                cursorGain <- cursor ksHighshelfGain
                disconnect cursorPlayBuf cursorHighshelf
                disconnect cursorHighshelf cursorGain
                destroy cursorHighshelf
                destroy cursorPlayBuf
                reset
                toAdd <- create (ksLowshelfCreate Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
