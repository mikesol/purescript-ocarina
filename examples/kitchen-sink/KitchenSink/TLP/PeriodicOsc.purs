module WAGS.Example.KitchenSink.TLP.PeriodicOsc where

import Prelude

import Data.Either (Either(..))
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.SawtoothOsc (doSawtoothOsc)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.PeriodicOsc (PeriodicOscUniverse, deltaKsPeriodicOsc, ksPeriodicOscGain, ksPeriodicOscPeriodicOsc)
import WAGS.Graph.AudioUnit (OnOff(..), SawtoothOsc(..))


doPeriodicOsc :: forall proof iu cb. StepSig (PeriodicOscUniverse cb) proof iu
doPeriodicOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksPeriodicOsc.end then
          Right (change (deltaKsPeriodicOsc time) $> lsig)
        else
          Left
            $ inSitu doSawtoothOsc WAGS.do
                cursorPeriodicOsc <- cursor ksPeriodicOscPeriodicOsc
                cursorGain <- cursor ksPeriodicOscGain
                disconnect cursorPeriodicOsc cursorGain
                destroy cursorPeriodicOsc
                reset
                toAdd <- create (SawtoothOsc On 440.0)
                connect toAdd cursorGain
                withProof pr lsig
