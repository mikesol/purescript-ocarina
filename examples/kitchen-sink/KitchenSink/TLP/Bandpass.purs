module WAGS.Example.KitchenSink.TLP.Bandpass where

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
import WAGS.Example.KitchenSink.TLP.Notch (doNotch)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Bandpass (BandpassGraph, deltaKsBandpass)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Notch (ksNotchCreate)
import WAGS.Run (SceneI(..))

doBandpass :: forall proof. StepSig BandpassGraph proof
doBandpass =
  ibranch \(SceneI { time }) lsig ->
    if time % pieceTime < timing.ksBandpass.end then
      Right (deltaKsBandpass time $> lsig)
    else
      Left
        $ icont doNotch Ix.do
            let
              cursorBandpass = Proxy :: _ "bandpass"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorBandpass }
            idisconnect { source: cursorBandpass, dest: cursorGain }
            idestroy cursorBandpass
            idestroy cursorPlayBuf
            icreate ksNotchCreate
            iconnect { source: Proxy :: _ "notch", dest: cursorGain }
            ipure lsig
