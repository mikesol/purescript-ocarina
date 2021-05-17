module WAGS.Example.KitchenSink.TLP.Delay where

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
import WAGS.Example.KitchenSink.TLP.Feedback (doFeedback)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Delay (DelayUniverse, deltaKsDelay, ksDelayDelay, ksDelayGain, ksDelayMix, ksDelayPlaybuf)
import WAGS.Example.KitchenSink.Types.Feedback (ksFeedbackCreate)

doDelay :: forall proof iu cb. StepSig (DelayUniverse cb) proof iu
doDelay =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksDelay.end then
          Right (change (deltaKsDelay time) $> lsig)
        else
          Left
            $ inSitu doFeedback WAGS.do
                cursorDelay <- cursor ksDelayDelay
                cursorPlayBuf <- cursor ksDelayPlaybuf
                cursorMix <- cursor ksDelayMix
                cursorGain <- cursor ksDelayGain
                disconnect cursorPlayBuf cursorMix
                disconnect cursorPlayBuf cursorDelay
                disconnect cursorDelay cursorMix
                disconnect cursorMix cursorGain
                destroy cursorPlayBuf
                destroy cursorDelay
                destroy cursorMix
                reset
                toAdd <- create (ksFeedbackCreate Identity Identity Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
