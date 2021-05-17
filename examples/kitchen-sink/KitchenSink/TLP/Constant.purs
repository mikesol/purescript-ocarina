module WAGS.Example.KitchenSink.TLP.Constant where

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
import WAGS.Example.KitchenSink.TLP.DynamicsCompressor (doDynamicsCompressor)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Constant (ConstantUniverse, deltaKsConstant, ksConstantGain, ksConstantConstant)
import WAGS.Example.KitchenSink.Types.DynamicsCompressor (ksDynamicsCompressorCreate)

doConstant :: forall proof iu cb. StepSig (ConstantUniverse cb) proof iu
doConstant =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksConstant.end then
          Right (change (deltaKsConstant time) $> lsig)
        else
          Left
            $ inSitu doDynamicsCompressor WAGS.do
                cursorConstant <- cursor ksConstantConstant
                cursorGain <- cursor ksConstantGain
                disconnect cursorConstant cursorGain
                destroy cursorConstant
                reset
                toAdd <- create (ksDynamicsCompressorCreate Identity Identity)
                connect toAdd cursorGain
                withProof pr lsig
