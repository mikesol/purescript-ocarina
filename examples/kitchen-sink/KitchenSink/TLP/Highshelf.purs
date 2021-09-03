module WAGS.Example.KitchenSink.TLP.Highshelf where

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
import WAGS.Example.KitchenSink.TLP.Lowshelf (doLowshelf)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highshelf (HighshelfGraph, deltaKsHighshelf)
import WAGS.Example.KitchenSink.Types.Lowshelf (ksLowshelfCreate)
import WAGS.Run (SceneI(..))

doHighshelf :: forall proof. StepSig HighshelfGraph proof
doHighshelf =
  ibranch \(SceneI { time, world }) lsig ->
    if time % pieceTime < timing.ksHighshelf.end then
      Right (deltaKsHighshelf world time $> lsig)
    else
      Left
        $ icont doLowshelf Ix.do
            let
              cursorHighshelf = Proxy :: _ "highshelf"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorHighshelf }
            idisconnect { source: cursorHighshelf, dest: cursorGain }
            idestroy cursorHighshelf
            idestroy cursorPlayBuf
            icreate (ksLowshelfCreate world)
            iconnect { source: Proxy :: _ "lowshelf", dest: cursorGain }
            ipure lsig
