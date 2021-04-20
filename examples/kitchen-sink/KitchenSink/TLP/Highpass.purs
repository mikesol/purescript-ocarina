module WAGS.Example.KitchenSink.TLP.Highpass where

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
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..))
import WAGS.Example.KitchenSink.Timing (pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Highpass (HighpassUniverse, deltaKsHighpass, ksHighpassBegins, ksHighpassGain, ksHighpassHighpass, ksHighpassPlaybuf)
import WAGS.Graph.Constructors (OnOff(..), SinOsc(..))
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doHighpass ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (HighpassUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doHighpass =
  branch \l@(LoopSig lsig) -> WAGS.do
    { time } <- env
    toRemove <- cursor ksHighpassHighpass
    toRemoveBuf <- cursor ksHighpassPlaybuf
    gn <- cursor ksHighpassGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < ksHighpassBegins then
          Left
            $ inSitu lsig WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemove
                destroy toRemoveBuf
                reset
                toAdd <- create (SinOsc On 440.0)
                connect toAdd gn
                withProof pr l
        else
          Right (change (deltaKsHighpass time) $> l)
