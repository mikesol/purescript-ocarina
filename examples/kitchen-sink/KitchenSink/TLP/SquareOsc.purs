module WAGS.Example.KitchenSink.TLP.SquareOsc where

import Prelude

import Control.Monad.Indexed (ipure, (:*>))
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, icont, imodifyRes)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.PeriodicOsc (doPeriodicOsc)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.PeriodicOsc (ksPeriodicOscCreate)
import WAGS.Example.KitchenSink.Types.SquareOsc (SquareOscGraph, deltaKsSquareOsc)

doSquareOsc :: forall proof. StepSig SquareOscGraph proof
doSquareOsc =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksSquareOsc.end then
      Right
        ( imodifyRes (const "Playing a square osc")
            :*> ichange (deltaKsSquareOsc time)
            $> lsig
        )
    else
      Left
        $ icont doPeriodicOsc Ix.do
            let
              cursorSquareOsc = Proxy :: _ "squareOsc"
            idisconnect { source: cursorSquareOsc, dest: cursorGain }
            idestroy cursorSquareOsc
            icreate ksPeriodicOscCreate
            iconnect { source: Proxy :: _ "periodicOsc", dest: cursorGain }
            ipure lsig
