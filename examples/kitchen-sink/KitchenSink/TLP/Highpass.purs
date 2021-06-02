module WAGS.Example.KitchenSink.TLP.Highpass where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, iwag)
import WAGS.Control.Indexed (wag)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.Microphone (doMicrophone)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highpass (HighpassGraph, deltaKsHighpass)
import WAGS.Example.KitchenSink.Types.Microphone (ksMicrophoneCreate)

doHighpass :: forall proof. StepSig HighpassGraph proof
doHighpass =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksHighpass.end then
      Right (ichange (deltaKsHighpass time) $> lsig)
    else
      Left
        $ iwag Ix.do
            let
              cursorHighpass = Proxy :: _ "highpass"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorHighpass }
            idisconnect { source: cursorHighpass, dest: cursorGain }
            idestroy cursorHighpass
            idestroy cursorPlayBuf
            icreate ksMicrophoneCreate
            iconnect { source: Proxy :: _ "microphone", dest: cursorGain }
            doMicrophone <$> wag lsig
