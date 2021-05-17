module WAGS.Example.KitchenSink.TLP.DynamicsCompressor where

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
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..), StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.DynamicsCompressor (DynamicsCompressorGraph, deltaKsDynamicsCompressor)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SinOsc (ksSinOscCreate)

doDynamicsCompressor :: forall proof iu. StepSig DynamicsCompressorGraph proof { | iu }
doDynamicsCompressor =
  branch \l@(LoopSig lsig) -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksDynamicsCompressor.begin then
          Left
            $ inSitu lsig WAGS.do
                let
                  cursorCompressor = Proxy :: Proxy "compressor"

                  cursorPlayBuf = Proxy :: Proxy "buf"

                disconnect cursorPlayBuf cursorCompressor
                disconnect cursorCompressor cursorGain
                destroy cursorCompressor
                destroy cursorPlayBuf
                create ksSinOscCreate
                connect (Proxy :: _ "sinOsc") cursorGain
                withProof pr l
        else
          Right (change (deltaKsDynamicsCompressor time) $> l)
