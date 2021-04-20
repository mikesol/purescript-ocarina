module WAGS.Example.KitchenSink.TLP.Allpass where

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
import WAGS.Example.KitchenSink.Timing (ksAllpassIntegral, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (AllpassUniverse, ksAllpassAllpass, ksAllpassGain, ksAllpassPlaybuf, deltaKsAllpass)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Highpass (ksHighpassCreate)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doAllpass ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (AllpassUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doAllpass =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksAllpassAllpass
    toRemoveBuf <- cursor ksAllpassPlaybuf
    gn <- cursor ksAllpassGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < ksAllpassIntegral then
          Right (change (deltaKsAllpass time) $> lsig)
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
