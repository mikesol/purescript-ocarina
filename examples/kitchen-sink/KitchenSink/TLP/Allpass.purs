module WAGS.Example.KitchenSink.TLP.Allpass where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch)
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.Lowpass (doLowpass)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (AllpassGraph, deltaKsAllpass)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Lowpass (ksLowpassCreate)
import WAGS.Graph.Optionals (lowpass_, playBuf_)
import WAGS.Patch (patch)

doAllpass :: forall proof. StepSig AllpassGraph proof
doAllpass =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksAllpass.end then
          Right (change (deltaKsAllpass time) $> lsig)
        else
          Left
            $ inSitu doLowpass
                ( if lsig.iteration `mod` 2 == 0 then WAGS.do
                    let
                      cursorAllpass = Proxy :: _ "allpass"

                      cursorPlayBuf = Proxy :: _ "buf"
                    disconnect cursorPlayBuf cursorAllpass
                    disconnect cursorAllpass cursorGain
                    destroy cursorAllpass
                    destroy cursorPlayBuf
                    create ksLowpassCreate
                    connect (Proxy :: _ "lowpass") cursorGain
                    withProof pr lsig
                  -- tests patch
                  else WAGS.do
                    patch
                    ivoid
                      $ change
                          { lowpass: lowpass_ { freq: 300.0 }
                          , buf: playBuf_ "my-buffer"
                          }
                    withProof pr lsig
                )
