module WAGS.Example.KitchenSink.TLP.PeriodicOsc where

import Prelude
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch)
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.SawtoothOsc (doSawtoothOsc)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.PeriodicOsc (PeriodicOscGraph, deltaKsPeriodicOsc)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (frontloadSawtoothOsc, ksSawtoothOscCreate)

doPeriodicOsc :: forall proof. StepSig PeriodicOscGraph proof
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
                let
                  cursorPeriodicOsc = Proxy :: _ "periodicOsc"
                disconnect cursorPeriodicOsc cursorGain
                destroy cursorPeriodicOsc
                create ksSawtoothOscCreate
                connect (Proxy :: _ "sawtoothOsc") cursorGain
                frontloadSawtoothOsc
                withProof pr lsig
