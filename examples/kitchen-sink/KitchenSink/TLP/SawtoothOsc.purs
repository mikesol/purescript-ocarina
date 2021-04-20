module WAGS.Example.KitchenSink.TLP.SawtoothOsc where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
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
import WAGS.Example.KitchenSink.TLP.Allpass (doAllpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.Timing (phase5Integral, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (phase6Create)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (SawtoothOscUniverse, deltaPhase5, phase5Gain, phase5SawtoothOsc)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doSawtoothOsc ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (SawtoothOscUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doSawtoothOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor phase5SawtoothOsc
    gn <- cursor phase5Gain
    pr <- proof
    withProof pr
      $ if time % pieceTime < phase5Integral then
          Right (change (deltaPhase5 time) $> lsig)
        else
          Left \thunk ->
            doAllpass WAGS.do
              thunk
              toAdd <- create (phase6Create Identity Identity)
              disconnect toRemove gn
              connect toAdd gn
              destroy toRemove
              withProof pr lsig
