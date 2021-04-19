module WAGS.Example.KitchenSink.TLP.PeriodicOsc where

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
import WAGS.Example.KitchenSink.TLP.SawtoothOsc (doSawtoothOsc)
import WAGS.Example.KitchenSink.Types (pieceTime, phase4Integral)
import WAGS.Example.KitchenSink.Types.PeriodicOsc (PeriodicOscUniverse, deltaPhase4, phase4Gain, phase4PeriodicOsc)
import WAGS.Graph.Constructors (OnOff(..), SawtoothOsc(..))
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doPeriodicOsc ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (PeriodicOscUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doPeriodicOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor phase4PeriodicOsc
    gn <- cursor phase4Gain
    pr <- proof
    withProof pr
      $ if time % pieceTime < phase4Integral then
          Right (change (deltaPhase4 time) $> lsig)
        else
          Left \thunk ->
            doSawtoothOsc WAGS.do
              thunk
              toAdd <- create (SawtoothOsc On 440.0)
              disconnect toRemove gn
              connect toAdd gn
              destroy toRemove
              withProof pr lsig
