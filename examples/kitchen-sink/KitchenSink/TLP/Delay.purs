module WAGS.Example.KitchenSink.TLP.Delay where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Effect (Effect)
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame, Scene)
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.Feedback (doFeedback)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Delay (DelayUniverse, deltaKsDelay, ksDelayDelay, ksDelayGain, ksDelayMix, ksDelayPlaybuf)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Feedback (ksFeedbackCreate)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doDelay ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (DelayUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doDelay =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemoveDelay <- cursor ksDelayDelay
    toRemoveBuf <- cursor ksDelayPlaybuf
    toRemoveMix <- cursor ksDelayMix
    gn <- cursor ksDelayGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksDelay.end then
          Right (change (deltaKsDelay time) $> lsig)
        else
          Left
            $ inSitu doFeedback WAGS.do
                disconnect toRemoveBuf toRemoveMix
                disconnect toRemoveBuf toRemoveDelay
                disconnect toRemoveDelay toRemoveMix
                disconnect toRemoveMix gn
                destroy toRemoveBuf
                destroy toRemoveDelay
                destroy toRemoveMix
                reset
                toAdd <- create (ksFeedbackCreate Identity Identity Identity Identity)
                connect toAdd gn
                withProof pr lsig
