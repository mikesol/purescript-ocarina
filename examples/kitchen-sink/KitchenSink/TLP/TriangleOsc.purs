module WAGS.Example.KitchenSink.TLP.TriangleOsc where

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
import WAGS.Example.KitchenSink.TLP.SquareOsc (doSquareOsc)
import WAGS.Example.KitchenSink.Types (pieceTime)
import WAGS.Example.KitchenSink.Types.SinOsc (phase1Time)
import WAGS.Example.KitchenSink.Types.TriangleOsc (TriangleOscUniverse, deltaPhase2, phase2Gain, phase2Time, phase2TriangleOsc)
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
    toRemove <- cursor phase2TriangleOsc
    gn <- cursor phase2Gain
    pr <- proof
    withProof pr
      $ if time % pieceTime < (phase1Time + phase2Time) then
          Right
            (change (deltaPhase2 time) $> lsig)
        else
          Left \thunk ->
            doSquareOsc WAGS.do
              thunk
              toAdd <- create (SquareOsc On 440.0)
              disconnect toRemove gn
              connect toAdd gn
              destroy toRemove
              withProof pr lsig
