module WAGS.Example.KitchenSink.TLP.Lowshelf where

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
import WAGS.Example.KitchenSink.TLP.Bandpass (doBandpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Bandpass (ksBandpassCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Lowshelf (LowshelfGraph, deltaKsLowshelf)

doLowshelf :: forall proof iu . StepSig LowshelfGraph proof {|iu}
doLowshelf =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksLowshelf.end then
          Right (change (deltaKsLowshelf time) $> lsig)
        else
          Left
            $ inSitu doBandpass WAGS.do
                let
                  cursorLowshelf = Proxy :: _ "lowshelf"

                  cursorPlayBuf = Proxy :: _ "buf"

                disconnect cursorPlayBuf cursorLowshelf
                disconnect cursorLowshelf cursorGain
                destroy cursorLowshelf
                destroy cursorPlayBuf
                create ksBandpassCreate
                connect (Proxy :: _ "bandpass") cursorGain
                withProof pr lsig
