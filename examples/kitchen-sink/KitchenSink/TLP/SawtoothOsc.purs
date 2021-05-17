module WAGS.Example.KitchenSink.TLP.SawtoothOsc where

import Prelude
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
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
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (SawtoothOscGraph, deltaKsSawtoothOsc)

doSawtoothOsc :: forall proof iu. StepSig SawtoothOscGraph proof { | iu }
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
                let
                  cursorSawtoothOsc = Proxy :: _ "sawtoothOsc"
                disconnect cursorSawtoothOsc cursorGain
                destroy cursorSawtoothOsc
                create ksAllpassCreate
                connect (Proxy :: _ "allpass") cursorGain
                withProof pr lsig
