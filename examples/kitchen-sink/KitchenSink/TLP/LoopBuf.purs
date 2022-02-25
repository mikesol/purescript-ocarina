module WAGS.Example.KitchenSink.TLP.LoopBuf where

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
import WAGS.Example.KitchenSink.TLP.StereoPanner (doStereoPanner)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.LoopBuf (LoopBufGraph, deltaKsLoopBuf)
import WAGS.Example.KitchenSink.Types.StereoPanner (ksStereoPannerCreate)
import WAGS.Run (BehavingScene(..))

doLoopBuf :: forall proof. StepSig LoopBufGraph proof
doLoopBuf =
  ibranch \(BehavingScene { time, world }) lsig ->
    if time % pieceTime < timing.ksLoopBuf.end then
      Right (deltaKsLoopBuf world time $> lsig)
    else
      Left
        $ icont doStereoPanner Ix.do
            let
              cursorLoopBuf = Proxy :: _ "loopBuf"
            idisconnect { source: cursorLoopBuf, dest: cursorGain }
            idestroy cursorLoopBuf
            icreate (ksStereoPannerCreate world)
            iconnect { source: Proxy :: _ "pan", dest: cursorGain }
            ipure lsig
