module WAGS.Example.KitchenSink.TLP.SinOsc where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame, Scene)
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.TLP.TriangleOsc (doTriangleOsc)
import WAGS.Example.KitchenSink.Types (pieceTime)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscUniverse, deltaPhase1, phase1Gain, phase1SinOsc, phase1Time)
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
    toRemove <- cursor phase1SinOsc
    gn <- cursor phase1Gain
    pr <- proof
    withProof pr
      $ if time % pieceTime < phase1Time then
          Right
            (change (deltaPhase1 time) $> lsig)
        else
          Left \thunk ->
            doTriangleOsc
              ( WAGS.do
                  thunk
                  toAdd <- create (TriangleOsc On 440.0)
                  disconnect toRemove gn
                  connect toAdd gn
                  destroy toRemove
                  withProof pr lsig
              )
