module WAGS.Example.KitchenSink.TLP.Bandpass where

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
import WAGS.Example.KitchenSink.TLP.Notch (doNotch)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Bandpass (BandpassGraph, deltaKsBandpass)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Notch (ksNotchCreate)

doBandpass :: forall proof. StepSig BandpassGraph proof
doBandpass =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksBandpass.end then
          Right (change (deltaKsBandpass time) $> lsig)
        else
          Left
            $ inSitu doNotch WAGS.do
                let
                  cursorBandpass = Proxy :: _ "bandpass"

                  cursorPlayBuf = Proxy :: _ "buf"
                disconnect cursorPlayBuf cursorBandpass
                disconnect cursorBandpass cursorGain
                destroy cursorBandpass
                destroy cursorPlayBuf
                create ksNotchCreate
                connect (Proxy :: _ "notch") cursorGain
                withProof pr lsig
