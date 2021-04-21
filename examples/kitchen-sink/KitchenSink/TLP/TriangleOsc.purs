module WAGS.Example.KitchenSink.TLP.TriangleOsc where

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
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.TLP.SquareOsc (doSquareOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.TriangleOsc (TriangleOscUniverse, deltaKsTriangleOsc, ksTriangleOscGain, ksTriangleOscTriangleOsc)
import WAGS.Graph.Constructors (OnOff(..), SquareOsc(..))
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doTriangleOsc ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (TriangleOscUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doTriangleOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksTriangleOscTriangleOsc
    gn <- cursor ksTriangleOscGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksTriangleOsc.end then
          Right (change (deltaKsTriangleOsc time) $> lsig)
        else
          Left
            $ inSitu doSquareOsc WAGS.do
                disconnect toRemove gn
                destroy toRemove
                reset
                toAdd <- create (SquareOsc On 440.0)
                connect toAdd gn
                withProof pr lsig
