module WAGS.Example.KitchenSink.TLP.TriangleOsc where

import Prelude
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
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
import WAGS.Example.KitchenSink.TLP.SquareOsc (doSquareOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SquareOsc (ksSquareOscCreate)
import WAGS.Example.KitchenSink.Types.TriangleOsc (TriangleOscGraph, deltaKsTriangleOsc)
import WAGS.Graph.Optionals (squareOsc_)
import WAGS.Patch (patch)

doTriangleOsc :: forall proof. StepSig TriangleOscGraph proof
doTriangleOsc =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksTriangleOsc.end then
          Right (change (deltaKsTriangleOsc time) $> lsig)
        else
          Left
            $ inSitu doSquareOsc
                ( if lsig.iteration `mod` 2 == 0 then WAGS.do
                    patch
                    ivoid $ change { squareOsc: squareOsc_ 220.0 }
                    withProof pr lsig
                  else WAGS.do
                    let
                      cursorTriangleOsc = Proxy :: _ "triangleOsc"

                      cursorRecorder = Proxy :: _ "recorder"
                    disconnect cursorRecorder cursorGain
                    disconnect cursorTriangleOsc cursorRecorder
                    destroy cursorTriangleOsc
                    destroy cursorRecorder
                    create ksSquareOscCreate
                    connect (Proxy :: _ "squareOsc") cursorGain
                    withProof pr lsig
                )
