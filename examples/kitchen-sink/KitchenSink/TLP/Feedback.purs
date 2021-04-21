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
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopBuf (doLoopBuf)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Feedback (FeedbackUniverse, deltaKsFeedback, ksFeedbackAttenuation, ksFeedbackDelay, ksFeedbackGain, ksFeedbackMix, ksFeedbackPlaybuf)
import WAGS.Graph.Optionals (loopBuf)

doFeedback :: forall proof iu cb. StepSig (FeedbackUniverse cb) proof iu
doFeedback =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    toRemoveDelay <- cursor ksFeedbackDelay
    toRemoveBuf <- cursor ksFeedbackPlaybuf
    toRemoveMix <- cursor ksFeedbackMix
    toRemoveAttenuation <- cursor ksFeedbackAttenuation
    gn <- cursor ksFeedbackGain
    withProof pr
      $ if time % pieceTime < timing.ksFeedback.end then
          Right (change (deltaKsFeedback time) $> lsig)
        else
          Left
            $ inSitu doLoopBuf WAGS.do
                disconnect toRemoveBuf toRemoveMix
                disconnect toRemoveDelay toRemoveMix
                disconnect toRemoveAttenuation toRemoveDelay
                disconnect toRemoveMix toRemoveAttenuation
                disconnect toRemoveMix gn
                destroy toRemoveBuf
                destroy toRemoveDelay
                destroy toRemoveMix
                destroy toRemoveAttenuation
                reset
                toAdd <-
                  create
                    $ loopBuf
                        { playbackRate: 1.0, start: 1.0, end: 2.5 }
                        (Proxy :: _ "my-buffer")
                connect toAdd gn
                withProof pr lsig
