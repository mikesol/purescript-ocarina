module WAGS.Example.KitchenSink.TLP.Constant where

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
import WAGS.Example.KitchenSink.TLP.DynamicsCompressor (doDynamicsCompressor)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Constant (ConstantGraph, deltaKsConstant)
import WAGS.Example.KitchenSink.Types.DynamicsCompressor (ksDynamicsCompressorCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)

doConstant :: forall proof iu. StepSig ConstantGraph proof { | iu }
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
                let
                  cursorConstant = Proxy :: _ "constant"

                disconnect cursorConstant cursorGain
                destroy cursorConstant
                create ksDynamicsCompressorCreate
                connect (Proxy :: _ "compressor") cursorGain
                withProof pr lsig
