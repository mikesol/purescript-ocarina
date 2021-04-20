module WAGS.Example.KitchenSink.TLP.Lowshelf where

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
import WAGS.Example.KitchenSink.TLP.Bandpass (doBandpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.Timing (ksLowshelfIntegral, pieceTime)
import WAGS.Example.KitchenSink.Types.Bandpass (ksBandpassCreate)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Lowshelf (LowshelfUniverse, ksLowshelfLowshelf, ksLowshelfGain, ksLowshelfPlaybuf, deltaKsLowshelf)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doLowshelf ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (LowshelfUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doLowshelf =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksLowshelfLowshelf
    toRemoveBuf <- cursor ksLowshelfPlaybuf
    gn <- cursor ksLowshelfGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < ksLowshelfIntegral then
          Right (change (deltaKsLowshelf time) $> lsig)
        else
          Left
            $ inSitu doBandpass WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemove
                destroy toRemoveBuf
                reset
                toAdd <- create (ksBandpassCreate Identity Identity)
                connect toAdd gn
                withProof pr lsig
