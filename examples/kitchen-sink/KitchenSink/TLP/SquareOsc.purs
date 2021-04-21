module WAGS.Example.KitchenSink.TLP.SquareOsc where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Math ((%))
import Type.Proxy (Proxy(..))
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
import WAGS.Example.KitchenSink.TLP.PeriodicOsc (doPeriodicOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.SquareOsc (SquareOscUniverse, deltaKsSquareOsc, ksSquareOscGain, ksSquareOscSquareOsc)
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
    toRemove <- cursor ksSquareOscSquareOsc
    gn <- cursor ksSquareOscGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksSquareOsc.end then
          Right (change (deltaKsSquareOsc time) $> lsig)
        else
          Left
            $ inSitu doPeriodicOsc WAGS.do
                disconnect toRemove gn
                destroy toRemove
                reset
                toAdd <- create (PeriodicOsc (Proxy :: Proxy "my-wave") On 440.0)
                connect toAdd gn
                withProof pr lsig
