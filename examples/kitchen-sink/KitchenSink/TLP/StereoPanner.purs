module WAGS.Example.KitchenSink.TLP.StereoPanner where

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
import WAGS.Example.KitchenSink.TLP.Constant (doConstant)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Constant (ksConstantCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.StereoPanner (StereoPannerGraph, deltaKsStereoPanner)

doStereoPanner :: forall proof. StepSig StereoPannerGraph proof
doStereoPanner =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksStereoPanner.end then
          Right (change (deltaKsStereoPanner time) $> lsig)
        else
          Left
            $ inSitu doConstant WAGS.do
                let
                  cursorStereoPanner = Proxy :: _ "pan"

                  cursorStereoPannerBuf = Proxy :: _ "buf"
                disconnect cursorStereoPannerBuf cursorStereoPanner
                disconnect cursorStereoPanner cursorGain
                destroy cursorStereoPanner
                destroy cursorStereoPannerBuf
                create ksConstantCreate
                connect (Proxy :: _ "constant") cursorGain
                withProof pr lsig
