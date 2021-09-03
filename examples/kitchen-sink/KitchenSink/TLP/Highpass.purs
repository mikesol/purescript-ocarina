module WAGS.Example.KitchenSink.TLP.Highpass where

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
import WAGS.Example.KitchenSink.TLP.Microphone (doMicrophone)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highpass (HighpassGraph, deltaKsHighpass)
import WAGS.Example.KitchenSink.Types.Microphone (ksMicrophoneCreate)
import WAGS.Run (SceneI(..))

doHighpass :: forall proof. StepSig HighpassGraph proof
doHighpass =
  ibranch \(SceneI { time, world }) lsig ->
    if time % pieceTime < timing.ksHighpass.end then
      Right (deltaKsHighpass world time $> lsig)
    else
      Left
        $ icont doMicrophone Ix.do
            let
              cursorHighpass = Proxy :: _ "highpass"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorHighpass }
            idisconnect { source: cursorHighpass, dest: cursorGain }
            idestroy cursorHighpass
            idestroy cursorPlayBuf
            icreate (ksMicrophoneCreate world)
            iconnect { source: Proxy :: _ "microphone", dest: cursorGain }
            ipure lsig
