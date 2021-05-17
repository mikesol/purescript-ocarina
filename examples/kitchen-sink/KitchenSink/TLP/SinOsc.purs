module WAGS.Example.KitchenSink.TLP.SinOsc where

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
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.TriangleOsc (doTriangleOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscGraph, deltaKsSinOsc)
import WAGS.Example.KitchenSink.Types.TriangleOsc (ksTriangleOscCreate)

doSinOsc :: forall proof iu. StepSig SinOscGraph proof { | iu }
doSinOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksSinOsc.end then
          Right (change (deltaKsSinOsc time) $> lsig)
        else
          Left
            $ inSitu doTriangleOsc WAGS.do
                let
                  cursorSinOsc = Proxy :: _ "sinOsc"
                disconnect cursorSinOsc cursorGain
                destroy cursorSinOsc
                create ksTriangleOscCreate
                connect (Proxy :: _ "recorder") cursorGain
                withProof pr lsig
