module WAGS.Example.KitchenSink.TLP.SquareOsc where

import Prelude
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.PeriodicOsc (doPeriodicOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.SquareOsc (SquareOscUniverse, deltaKsSquareOsc, ksSquareOscGain, ksSquareOscSquareOsc)
import WAGS.Graph.AudioUnit (OnOff(..), PeriodicOsc(..))

doSquareOsc :: forall proof iu cb. StepSig (SquareOscUniverse cb) proof iu
doSquareOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    ivoid $ modifyRes (const "Playing a square osc")
    withProof pr
      $ if time % pieceTime < timing.ksSquareOsc.end then
          Right (change (deltaKsSquareOsc time) $> lsig)
        else
          Left
            $ inSitu doPeriodicOsc WAGS.do
                cursorSquareOsc <- cursor ksSquareOscSquareOsc
                cursorGain <- cursor ksSquareOscGain
                disconnect cursorSquareOsc cursorGain
                destroy cursorSquareOsc
                reset
                toAdd <- create (PeriodicOsc "my-wave" On 440.0)
                connect toAdd cursorGain
                withProof pr lsig
