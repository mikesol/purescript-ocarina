module WAGS.Example.KitchenSink.TLP.StereoPanner where

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
import WAGS.Example.KitchenSink.TLP.Constant (doConstant)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Constant (ksConstantCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.StereoPanner (StereoPannerGraph, deltaKsStereoPanner)

doStereoPanner :: forall proof. StepSig StereoPannerGraph proof
doStereoPanner =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksStereoPanner.end then
      Right (ichange (deltaKsStereoPanner time) $> lsig)
    else
      Left
        $ iwag Ix.do
            let
              cursorStereoPanner = Proxy :: _ "pan"

              cursorStereoPannerBuf = Proxy :: _ "buf"
            idisconnect { source: cursorStereoPannerBuf, dest: cursorStereoPanner }
            idisconnect { source: cursorStereoPanner, dest: cursorGain }
            idestroy cursorStereoPanner
            idestroy cursorStereoPannerBuf
            icreate ksConstantCreate
            iconnect { source: Proxy :: _ "constant", dest: cursorGain }
            doConstant <$> (wag lsig)
