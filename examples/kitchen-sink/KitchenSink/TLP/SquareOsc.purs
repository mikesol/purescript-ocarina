module WAGS.Example.KitchenSink.TLP.SquareOsc where

import Prelude
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch)
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.PeriodicOsc (doPeriodicOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.PeriodicOsc (ksPeriodicOscCreate)
import WAGS.Example.KitchenSink.Types.SquareOsc (SquareOscGraph, deltaKsSquareOsc)

doSquareOsc :: forall proof. StepSig SquareOscGraph proof
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
                let
                  cursorSquareOsc = Proxy :: _ "squareOsc"
                disconnect cursorSquareOsc cursorGain
                destroy cursorSquareOsc
                create ksPeriodicOscCreate
                connect (Proxy :: _ "periodicOsc") cursorGain
                withProof pr lsig
