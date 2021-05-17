module WAGS.Example.KitchenSink.TLP.SawtoothOsc where

import Prelude
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.Allpass (doAllpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (ksAllpassCreate)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (SawtoothOscUniverse, deltaKsSawtoothOsc, ksSawtoothOscGain, ksSawtoothOscSawtoothOsc)

doSawtoothOsc :: forall proof iu cb. StepSig (SawtoothOscUniverse cb) proof iu
doSawtoothOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksSawtoothOsc.end then
          Right (change (deltaKsSawtoothOsc time) $> lsig)
        else
          Left
            $ inSitu doAllpass WAGS.do
                cursorSawtoothOsc <- cursor ksSawtoothOscSawtoothOsc
                cursorGain <- cursor ksSawtoothOscGain
                disconnect cursorSawtoothOsc cursorGain
                destroy cursorSawtoothOsc
                reset
                toAdd <- create (ksAllpassCreate Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
