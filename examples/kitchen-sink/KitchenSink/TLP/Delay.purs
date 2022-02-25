module WAGS.Example.KitchenSink.TLP.Delay where

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
import WAGS.Example.KitchenSink.TLP.Feedback (doFeedback)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Delay (DelayGraph, deltaKsDelay)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Feedback (ksFeedbackCreate)
import WAGS.Run (BehavingScene(..))

doDelay :: forall proof. StepSig DelayGraph proof
doDelay =
  ibranch \(BehavingScene { time, world })lsig ->
    if time % pieceTime < timing.ksDelay.end then
      Right (deltaKsDelay world time $> lsig)
    else
      Left
        $ icont doFeedback Ix.do
            let
              cursorDelay = Proxy :: _ "delay"

              cursorBuf = Proxy :: _ "buf"

              cursorDMix = Proxy :: _ "dmix"
            idisconnect { source: cursorBuf, dest: cursorDelay }
            idisconnect { source: cursorDelay, dest: cursorDMix }
            idisconnect { source: cursorBuf, dest: cursorDMix }
            idisconnect { source: cursorDMix, dest: cursorGain }
            idestroy cursorBuf
            idestroy cursorDelay
            idestroy cursorDMix
            icreate (ksFeedbackCreate world)
            iconnect { source: Proxy :: _ "dmix", dest: cursorGain }
            ipure lsig
