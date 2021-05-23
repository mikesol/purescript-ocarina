module WAGS.Example.KitchenSink.TLP.Delay where

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
import WAGS.Example.KitchenSink.TLP.Feedback (doFeedback)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Delay (DelayGraph, deltaKsDelay)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Feedback (ksFeedbackCreate)

doDelay :: forall proof. StepSig DelayGraph proof
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
                let
                  cursorDelay = Proxy :: _ "delay"

                  cursorBuf = Proxy :: _ "buf"

                  cursorDMix = Proxy :: _ "dmix"
                disconnect cursorBuf cursorDelay
                disconnect cursorDelay cursorDMix
                disconnect cursorBuf cursorDMix
                disconnect cursorDMix cursorGain
                destroy cursorBuf
                destroy cursorDelay
                destroy cursorDMix
                create ksFeedbackCreate
                connect (Proxy :: _ "dmix") cursorGain
                withProof pr lsig
