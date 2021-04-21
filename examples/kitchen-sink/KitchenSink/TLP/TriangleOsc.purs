module WAGS.Example.KitchenSink.TLP.TriangleOsc where

import Prelude

import Data.Either (Either(..))
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.SquareOsc (doSquareOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.TriangleOsc (TriangleOscUniverse, deltaKsTriangleOsc, ksTriangleOscGain, ksTriangleOscRecorder, ksTriangleOscTriangleOsc)
import WAGS.Graph.Constructors (OnOff(..), SquareOsc(..))

doTriangleOsc :: forall proof iu cb. StepSig (TriangleOscUniverse cb) proof iu
doTriangleOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksTriangleOscTriangleOsc
    toRemoveRec <- cursor ksTriangleOscRecorder
    gn <- cursor ksTriangleOscGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksTriangleOsc.end then
          Right (change (deltaKsTriangleOsc time) $> lsig)
        else
          Left
            $ inSitu doSquareOsc WAGS.do
                disconnect toRemoveRec gn
                disconnect toRemove toRemoveRec
                destroy toRemove
                destroy toRemoveRec
                reset
                toAdd <- create (SquareOsc On 440.0)
                connect toAdd gn
                withProof pr lsig
