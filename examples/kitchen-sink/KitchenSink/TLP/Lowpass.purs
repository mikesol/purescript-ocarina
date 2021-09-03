module WAGS.Example.KitchenSink.TLP.Lowpass where

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
import WAGS.Example.KitchenSink.TLP.Highshelf (doHighshelf)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highshelf (ksHighshelfCreate)
import WAGS.Example.KitchenSink.Types.Lowpass (LowpassGraph, deltaKsLowpass)
import WAGS.Run (SceneI(..))

doLowpass :: forall proof. StepSig LowpassGraph proof
doLowpass =
  ibranch \(SceneI { time, world }) lsig ->
    if time % pieceTime < timing.ksLowpass.end then
      Right (deltaKsLowpass world time $> lsig)
    else
      Left
        $ icont doHighshelf Ix.do
            let
              cursorLowpass = Proxy :: _ "lowpass"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorLowpass }
            idisconnect { source: cursorLowpass, dest: cursorGain }
            idestroy cursorLowpass
            idestroy cursorPlayBuf
            icreate (ksHighshelfCreate world)
            iconnect { source: Proxy :: _ "highshelf", dest: cursorGain }
            ipure lsig
