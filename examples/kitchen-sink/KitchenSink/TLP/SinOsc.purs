module WAGS.Example.KitchenSink.TLP.SinOsc where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
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
import WAGS.Example.KitchenSink.TLP.TriangleOsc (doTriangleOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscGraph, deltaKsSinOsc)
import WAGS.Example.KitchenSink.Types.TriangleOsc (ksTriangleOscCreate)

doSinOsc :: forall proof. StepSig SinOscGraph proof
doSinOsc =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksSinOsc.end then
      Right (ichange (deltaKsSinOsc time) $> lsig)
    else
      Left
        $ iwag Ix.do
            let
              cursorSinOsc = Proxy :: _ "sinOsc"
            idisconnect { source: cursorSinOsc, dest: cursorGain }
            idestroy cursorSinOsc
            icreate ksTriangleOscCreate
            iconnect { source: Proxy :: _ "recorder", dest: cursorGain }
            doTriangleOsc <$> wag lsig
