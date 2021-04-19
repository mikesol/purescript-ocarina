module WAGS.Example.KitchenSink.TLP.SquareOsc where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Math ((%))
import Type.Proxy (Proxy(..))
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
import WAGS.Example.KitchenSink.TLP.PeriodicOsc (doPeriodicOsc)
import WAGS.Example.KitchenSink.Types (pieceTime)
import WAGS.Example.KitchenSink.Types.SinOsc (phase1Time)
import WAGS.Example.KitchenSink.Types.SquareOsc (SquareOscUniverse, deltaPhase3, phase3Gain, phase3SquareOsc, phase3Time)
import WAGS.Example.KitchenSink.Types.TriangleOsc (phase2Time)
import WAGS.Graph.Constructors (OnOff(..), PeriodicOsc(..))
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doSquareOsc ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (SquareOscUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doSquareOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor phase3SquareOsc
    gn <- cursor phase3Gain
    pr <- proof
    withProof pr
      $ if time % pieceTime < (phase1Time + phase2Time + phase3Time) then
          Right
            (change (deltaPhase3 time) $> lsig)
        else
          Left \thunk ->
            doPeriodicOsc
              ( WAGS.do
                  thunk
                  toAdd <- create (PeriodicOsc (Proxy :: Proxy "my-wave") On 440.0)
                  disconnect toRemove gn
                  connect toAdd gn
                  destroy toRemove
                  withProof pr lsig
              )
