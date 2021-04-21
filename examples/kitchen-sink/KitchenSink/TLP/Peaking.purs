module WAGS.Example.KitchenSink.TLP.Peaking where

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
import WAGS.Example.KitchenSink.TLP.Highpass (doHighpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Highpass (ksHighpassCreate)
import WAGS.Example.KitchenSink.Types.Peaking (PeakingUniverse, ksPeakingPeaking, ksPeakingGain, ksPeakingPlaybuf, deltaKsPeaking)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doPeaking ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (PeakingUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doPeaking =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksPeakingPeaking
    toRemoveBuf <- cursor ksPeakingPlaybuf
    gn <- cursor ksPeakingGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksPeaking.end then
          Right (change (deltaKsPeaking time) $> lsig)
        else
          Left
            $ inSitu doHighpass WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemove
                destroy toRemoveBuf
                reset
                toAdd <- create (ksHighpassCreate Identity Identity)
                connect toAdd gn
                withProof pr lsig
