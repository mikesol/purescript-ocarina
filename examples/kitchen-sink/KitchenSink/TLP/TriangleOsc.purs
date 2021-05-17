module WAGS.Example.KitchenSink.TLP.TriangleOsc where

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
import WAGS.Example.KitchenSink.TLP.SquareOsc (doSquareOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.TriangleOsc (TriangleOscUniverse, deltaKsTriangleOsc, ksTriangleOscGain, ksTriangleOscRecorder, ksTriangleOscTriangleOsc)
import WAGS.Graph.AudioUnit (OnOff(..), SquareOsc(..))

doTriangleOsc :: forall proof iu cb. StepSig (TriangleOscUniverse cb) proof iu
doTriangleOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksTriangleOsc.end then
          Right (change (deltaKsTriangleOsc time) $> lsig)
        else
          Left
            $ inSitu doSquareOsc WAGS.do
                cursorTriangleOsc <- cursor ksTriangleOscTriangleOsc
                cursorRecorder <- cursor ksTriangleOscRecorder
                cursorGain <- cursor ksTriangleOscGain
                disconnect cursorRecorder cursorGain
                disconnect cursorTriangleOsc cursorRecorder
                destroy cursorTriangleOsc
                destroy cursorRecorder
                reset
                toAdd <- create (SquareOsc On 440.0)
                connect toAdd cursorGain
                withProof pr lsig
