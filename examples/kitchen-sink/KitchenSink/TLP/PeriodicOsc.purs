module WAGS.Example.KitchenSink.TLP.PeriodicOsc where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, icont)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.SawtoothOsc (doSawtoothOsc)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.PeriodicOsc (PeriodicOscGraph, deltaKsPeriodicOsc)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (frontloadSawtoothOsc, ksSawtoothOscCreate)

doPeriodicOsc :: forall proof. StepSig PeriodicOscGraph proof
doPeriodicOsc =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksPeriodicOsc.end then
      Right (ichange (deltaKsPeriodicOsc time) $> lsig)
    else
      Left
        $ icont doSawtoothOsc Ix.do
            let
              cursorPeriodicOsc = Proxy :: _ "periodicOsc"
            idisconnect { source: cursorPeriodicOsc, dest: cursorGain }
            idestroy cursorPeriodicOsc
            icreate ksSawtoothOscCreate
            iconnect { source: Proxy :: _ "sawtoothOsc", dest: cursorGain }
            frontloadSawtoothOsc
            ipure lsig
