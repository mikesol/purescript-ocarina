module WAGS.Example.KitchenSink.TLP.TriangleOsc where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, iwag)
import WAGS.Control.Indexed (wag)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.SquareOsc (doSquareOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SquareOsc (ksSquareOscCreate)
import WAGS.Example.KitchenSink.Types.TriangleOsc (TriangleOscGraph, deltaKsTriangleOsc)
import WAGS.Graph.Optionals (squareOsc_)
import WAGS.Patch (ipatch)

doTriangleOsc :: forall proof. StepSig TriangleOscGraph proof
doTriangleOsc =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksTriangleOsc.end then
      Right (ichange (deltaKsTriangleOsc time) $> lsig)
    else if lsig.iteration `mod` 2 == 0 then
      Left
        $ iwag Ix.do
            ipatch
            ivoid $ ichange { squareOsc: squareOsc_ 220.0 }
            doSquareOsc <$> wag lsig
    else
      Left
        $ iwag Ix.do
            let
              cursorTriangleOsc = Proxy :: _ "triangleOsc"

              cursorRecorder = Proxy :: _ "recorder"
            idisconnect { source: cursorRecorder, dest: cursorGain }
            idisconnect { source: cursorTriangleOsc, dest: cursorRecorder }
            idestroy cursorTriangleOsc
            idestroy cursorRecorder
            icreate ksSquareOscCreate
            iconnect { source: Proxy :: _ "squareOsc", dest: cursorGain }
            doSquareOsc <$> wag lsig
