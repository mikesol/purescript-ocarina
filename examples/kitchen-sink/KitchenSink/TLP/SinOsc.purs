module WAGS.Example.KitchenSink.TLP.SinOsc where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, icont)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.TriangleOsc (doTriangleOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscGraph, deltaKsSinOsc)
import WAGS.Example.KitchenSink.Types.TriangleOsc (ksTriangleOscCreate)
import WAGS.Run (SceneI(..))

doSinOsc :: forall proof. StepSig SinOscGraph proof
doSinOsc =
  ibranch \(SceneI { time }) lsig ->
    if time % pieceTime < timing.ksSinOsc.end then
      Right (deltaKsSinOsc time $> lsig)
    else
      Left
        $ icont doTriangleOsc Ix.do
            let
              cursorSinOsc = Proxy :: _ "sinOsc"
            idisconnect { source: cursorSinOsc, dest: cursorGain }
            idestroy cursorSinOsc
            icreate ksTriangleOscCreate
            iconnect { source: Proxy :: _ "recorder", dest: cursorGain }
            ipure lsig
