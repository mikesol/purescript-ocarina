module WAGS.Example.KitchenSink.TLP.DynamicsCompressor where

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
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..), StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.DynamicsCompressor (DynamicsCompressorUniverse, deltaKsDynamicsCompressor, ksDynamicsCompressorGain, ksDynamicsCompressorDynamicsCompressor, ksDynamicsCompressorPlaybuf)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Graph.Constructors (OnOff(..), SinOsc(..))

doDynamicsCompressor :: forall proof iu cb. StepSig (DynamicsCompressorUniverse cb) proof iu
doDynamicsCompressor =
  branch \l@(LoopSig lsig) -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksDynamicsCompressor.begin then
          Left
            $ inSitu lsig WAGS.do
                cursorCompressor <- cursor ksDynamicsCompressorDynamicsCompressor
                cursorPlayBuf <- cursor ksDynamicsCompressorPlaybuf
                cursorGain <- cursor ksDynamicsCompressorGain
                disconnect cursorPlayBuf cursorCompressor
                disconnect cursorCompressor cursorGain
                destroy cursorCompressor
                destroy cursorPlayBuf
                reset
                toAdd <- create (SinOsc On 440.0)
                connect toAdd cursorGain
                withProof pr l
        else
          Right (change (deltaKsDynamicsCompressor time) $> l)
