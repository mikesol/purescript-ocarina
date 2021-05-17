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
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscUniverse, deltaKsSinOsc, ksSinOscGain, ksSinOscSinOsc)
import WAGS.Graph.Optionals (recorder, triangleOsc)

doSinOsc :: forall proof iu cb. StepSig (SinOscUniverse cb) proof iu
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
                cursorSinOsc <- cursor ksSinOscSinOsc
                cursorGain <- cursor ksSinOscGain
                disconnect cursorSinOsc cursorGain
                destroy cursorSinOsc
                reset
                toAdd <- create (recorder (Proxy :: _ "my-recorder") (triangleOsc 440.0))
                connect toAdd cursorGain
                withProof pr lsig
