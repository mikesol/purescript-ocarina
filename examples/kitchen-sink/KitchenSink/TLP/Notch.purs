module WAGS.Example.KitchenSink.TLP.Notch where

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
import WAGS.Example.KitchenSink.TLP.Peaking (doPeaking)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Notch (NotchGraph, deltaKsNotch)
import WAGS.Example.KitchenSink.Types.Peaking (ksPeakingCreate)
import WAGS.Run (SceneI(..))

doNotch :: forall proof. StepSig NotchGraph proof
doNotch =
  ibranch \(SceneI { time, world }) lsig ->
    if time % pieceTime < timing.ksNotch.end then
      Right (deltaKsNotch world time $> lsig)
    else
      Left
        $ icont doPeaking Ix.do
            let
              cursorNotch = Proxy :: _ "notch"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorNotch }
            idisconnect { source: cursorNotch, dest: cursorGain }
            idestroy cursorNotch
            idestroy cursorPlayBuf
            icreate (ksPeakingCreate world)
            iconnect { source: Proxy :: _ "peaking", dest: cursorGain }
            ipure lsig
