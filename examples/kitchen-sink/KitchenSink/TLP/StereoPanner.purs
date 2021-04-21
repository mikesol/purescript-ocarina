module WAGS.Example.KitchenSink.TLP.StereoPanner where

import Prelude

import Data.Either (Either(..))
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
import WAGS.Example.KitchenSink.TLP.Constant (doConstant)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.StereoPanner (StereoPannerUniverse, deltaKsStereoPanner, ksStereoPannerStereoPanner, ksStereoPannerGain, ksStereoPannerPlaybuf)
import WAGS.Graph.Optionals (constant)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doStereoPanner ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (StereoPannerUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doStereoPanner =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksStereoPannerStereoPanner
    toRemoveBuf <- cursor ksStereoPannerPlaybuf
    gn <- cursor ksStereoPannerGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksStereoPanner.end then
          Right (change (deltaKsStereoPanner time) $> lsig)
        else
          Left
            $ inSitu doConstant WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemove
                destroy toRemoveBuf
                reset
                toAdd <- create (constant 0.0)
                connect toAdd gn
                withProof pr lsig
