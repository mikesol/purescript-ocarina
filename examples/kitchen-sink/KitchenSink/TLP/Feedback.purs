module WAGS.Example.KitchenSink.TLP.Feedback where

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
import WAGS.Example.KitchenSink.TLP.DynamicsCompressor (doDynamicsCompressor)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.DynamicsCompressor (ksDynamicsCompressorCreate)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Feedback (FeedbackUniverse, deltaKsFeedback, ksFeedbackAttenuation, ksFeedbackDelay, ksFeedbackGain, ksFeedbackMix, ksFeedbackPlaybuf)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doFeedback ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (FeedbackUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
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
            $ inSitu doDynamicsCompressor WAGS.do
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
                toAdd <- create (ksDynamicsCompressorCreate Identity Identity)
                connect toAdd gn
                withProof pr lsig
