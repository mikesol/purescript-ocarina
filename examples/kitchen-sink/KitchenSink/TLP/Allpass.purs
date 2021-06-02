module WAGS.Example.KitchenSink.TLP.Allpass where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
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
import WAGS.Example.KitchenSink.TLP.Lowpass (doLowpass)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (AllpassGraph, deltaKsAllpass)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Lowpass (ksLowpassCreate)
import WAGS.Graph.Optionals (lowpass_, playBuf_)
import WAGS.Patch (ipatch)

doAllpass :: forall proof. StepSig AllpassGraph proof
doAllpass =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksAllpass.end then
      Right (ichange (deltaKsAllpass time) $> lsig)
    else if lsig.iteration `mod` 2 == 0 then
      Left
        $ iwag Ix.do
            let
              cursorAllpass = Proxy :: _ "allpass"

              cursorPlayBuf = Proxy :: _ "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorAllpass }
            idisconnect { source: cursorAllpass, dest: cursorGain }
            idestroy cursorAllpass
            idestroy cursorPlayBuf
            icreate ksLowpassCreate
            iconnect { source: Proxy :: _ "lowpass", dest: cursorGain }
            doLowpass <$> wag lsig
    else
      Left
        $ iwag Ix.do
            ipatch
            ivoid
              $ ichange
                  { lowpass: lowpass_ { freq: 300.0 }
                  , buf: playBuf_ "my-buffer"
                  }
            doLowpass <$> wag lsig
