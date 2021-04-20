module WAGS.Example.KitchenSink.TLP.DynamicsCompressor where

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
import WAGS.Example.KitchenSink.Types.DynamicsCompressor (DynamicsCompressorUniverse, deltaKsDynamicsCompressor, ksDynamicsCompressorBegins, ksDynamicsCompressorGain, ksDynamicsCompressorDynamicsCompressor, ksDynamicsCompressorPlaybuf)
import WAGS.Graph.Constructors (OnOff(..), SinOsc(..))
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doDynamicsCompressor ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (DynamicsCompressorUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doDynamicsCompressor =
  branch \l@(LoopSig lsig) -> WAGS.do
    { time } <- env
    toRemove <- cursor ksDynamicsCompressorDynamicsCompressor
    toRemoveBuf <- cursor ksDynamicsCompressorPlaybuf
    gn <- cursor ksDynamicsCompressorGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < ksDynamicsCompressorBegins then
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
          Right (change (deltaKsDynamicsCompressor time) $> l)
