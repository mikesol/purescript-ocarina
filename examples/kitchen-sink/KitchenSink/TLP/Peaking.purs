module WAGS.Example.KitchenSink.TLP.Peaking where

import Prelude

import Control.Monad.Indexed (ipure, (:*>))
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, imodifyRes, icont)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.Highpass (doHighpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highpass (ksHighpassCreate)
import WAGS.Example.KitchenSink.Types.Peaking (PeakingGraph, deltaKsPeaking)
import WAGS.Run (SceneI(..))

doPeaking :: forall proof. StepSig PeakingGraph proof
doPeaking =
  ibranch \(SceneI { time, world }) lsig ->
    if time % pieceTime < timing.ksPeaking.end then
      Right
        $ imodifyRes (const $ "Using a peaking filter")
        :*> deltaKsPeaking world time
        $> lsig
    else
      Left
        $ icont doHighpass Ix.do
            let
              cursorPeaking = Proxy :: _ "peaking"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorPeaking }
            idisconnect { source: cursorPeaking, dest: cursorGain }
            idestroy cursorPeaking
            idestroy cursorPlayBuf
            icreate (ksHighpassCreate world)
            iconnect { source: Proxy :: _ "highpass", dest: cursorGain }
            ipure lsig
