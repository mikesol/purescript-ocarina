module WAGS.Example.KitchenSink.TLP.SinOsc where

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
import WAGS.Example.KitchenSink.TLP.TriangleOsc (doTriangleOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscUniverse, deltaKsSinOsc, ksSinOscGain, ksSinOscSinOsc)
import WAGS.Graph.Constructors (OnOff(..), TriangleOsc(..))
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doSinOsc ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (SinOscUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doSinOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksSinOscSinOsc
    gn <- cursor ksSinOscGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksSinOsc.end then
          Right (change (deltaKsSinOsc time) $> lsig)
        else
          Left
            $ inSitu doTriangleOsc WAGS.do
                disconnect toRemove gn
                destroy toRemove
                reset
                toAdd <- create (TriangleOsc On 440.0)
                connect toAdd gn
                withProof pr lsig
