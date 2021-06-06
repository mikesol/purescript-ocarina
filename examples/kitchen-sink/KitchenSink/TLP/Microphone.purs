module WAGS.Example.KitchenSink.TLP.Microphone where

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
import WAGS.Example.KitchenSink.TLP.WaveShaper (doWaveShaper)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Microphone (MicrophoneGraph, deltaKsMicrophone)
import WAGS.Example.KitchenSink.Types.WaveShaper (ksWaveShaperCreate)

doMicrophone :: forall proof. StepSig MicrophoneGraph proof
doMicrophone =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksMicrophone.end then
      Right (deltaKsMicrophone time $> lsig)
    else
      Left
        $ icont doWaveShaper Ix.do
            let
              cursorMicrophone = Proxy :: _ "microphone"
            idisconnect { source: cursorMicrophone, dest: cursorGain }
            idestroy cursorMicrophone
            icreate ksWaveShaperCreate
            iconnect { source: Proxy :: _ "waveShaper", dest: cursorGain }
            ipure lsig
