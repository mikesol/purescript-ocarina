module WAGS.Example.KitchenSink.TLP.Feedback where

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
import WAGS.Example.KitchenSink.TLP.LoopBuf (doLoopBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Feedback (FeedbackGraph, deltaKsFeedback)
import WAGS.Example.KitchenSink.Types.LoopBuf (ksLoopBufCreate)

doFeedback :: forall proof iu. StepSig FeedbackGraph proof { | iu }
doFeedback =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksFeedback.end then
          Right (change (deltaKsFeedback time) $> lsig)
        else
          Left
            $ inSitu doLoopBuf WAGS.do
                let
                  cursorDmix = Proxy :: _ "dmix"

                  cursorDelay = Proxy :: _ "delay"

                  cursorBuf = Proxy :: _ "buf"

                  cursorHighpass = Proxy :: _ "highpass"
                disconnect cursorDmix cursorHighpass
                disconnect cursorHighpass cursorDelay
                disconnect cursorDelay cursorDmix
                disconnect cursorBuf cursorDmix
                disconnect cursorDmix cursorGain
                destroy cursorBuf
                destroy cursorDmix
                destroy cursorDelay
                destroy cursorHighpass
                create ksLoopBufCreate
                connect (Proxy :: _ "loopBuf") cursorGain
                withProof pr lsig
