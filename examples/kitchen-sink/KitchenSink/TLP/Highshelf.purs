module WAGS.Example.KitchenSink.TLP.Highshelf where

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
import WAGS.Example.KitchenSink.TLP.Lowshelf (doLowshelf)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highshelf (HighshelfGraph, deltaKsHighshelf)
import WAGS.Example.KitchenSink.Types.Lowshelf (ksLowshelfCreate)

doHighshelf :: forall proof iu. StepSig HighshelfGraph proof { | iu }
doHighshelf =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksHighshelf.end then
          Right (change (deltaKsHighshelf time) $> lsig)
        else
          Left
            $ inSitu doLowshelf WAGS.do
                let
                  cursorHighshelf = Proxy :: _ "highshelf"

                  cursorPlayBuf = Proxy :: _ "buf"
                disconnect cursorPlayBuf cursorHighshelf
                disconnect cursorHighshelf cursorGain
                destroy cursorHighshelf
                destroy cursorPlayBuf
                create ksLowshelfCreate
                connect (Proxy :: _ "lowshelf") cursorGain
                withProof pr lsig
