module WAGS.Example.KitchenSink.TLP.Lowshelf where

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
import WAGS.Example.KitchenSink.TLP.Bandpass (doBandpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Bandpass (ksBandpassCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Lowshelf (LowshelfGraph, deltaKsLowshelf)
import WAGS.Run (SceneI(..))

doLowshelf :: forall proof. StepSig LowshelfGraph proof
doLowshelf =
  ibranch \(SceneI { time, world }) lsig ->
    if time % pieceTime < timing.ksLowshelf.end then
      Right (deltaKsLowshelf world time $> lsig)
    else
      Left
        $ icont doBandpass Ix.do
            let
              cursorLowshelf = Proxy :: _ "lowshelf"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorLowshelf }
            idisconnect { source: cursorLowshelf, dest: cursorGain }
            idestroy cursorLowshelf
            idestroy cursorPlayBuf
            icreate (ksBandpassCreate world)
            iconnect { source: Proxy :: _ "bandpass", dest: cursorGain }
            ipure lsig
