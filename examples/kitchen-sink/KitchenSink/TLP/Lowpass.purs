module WAGS.Example.KitchenSink.TLP.Lowpass where

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
import WAGS.Example.KitchenSink.TLP.Highshelf (doHighshelf)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.Timing (ksLowpassIntegral, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Highshelf (ksHighshelfCreate)
import WAGS.Example.KitchenSink.Types.Lowpass (LowpassUniverse, ksLowpassLowpass, ksLowpassGain, ksLowpassPlaybuf, deltaKsLowpass)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doLowpass ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (LowpassUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doLowpass =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksLowpassLowpass
    toRemoveBuf <- cursor ksLowpassPlaybuf
    gn <- cursor ksLowpassGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < ksLowpassIntegral then
          Right (change (deltaKsLowpass time) $> lsig)
        else
          Left
            $ inSitu doHighshelf WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemove
                destroy toRemoveBuf
                reset
                toAdd <- create (ksHighshelfCreate Identity Identity)
                connect toAdd gn
                withProof pr lsig