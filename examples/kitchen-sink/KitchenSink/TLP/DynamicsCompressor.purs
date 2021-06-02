module WAGS.Example.KitchenSink.TLP.DynamicsCompressor where

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
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..), StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.DynamicsCompressor (DynamicsCompressorGraph, deltaKsDynamicsCompressor)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SinOsc (ksSinOscCreate)

doDynamicsCompressor :: forall proof. StepSig DynamicsCompressorGraph proof
doDynamicsCompressor =
  ibranch \{ time } l@{ loop: LoopSig lsig, iteration } ->
    if time % pieceTime < timing.ksDynamicsCompressor.begin then
      Left
        $ iwag Ix.do
            let
              cursorCompressor = Proxy :: Proxy "compressor"

              cursorPlayBuf = Proxy :: Proxy "buf"
            idisconnect { source: cursorPlayBuf, dest: cursorCompressor }
            idisconnect { source: cursorCompressor, dest: cursorGain }
            idestroy cursorCompressor
            idestroy cursorPlayBuf
            icreate ksSinOscCreate
            iconnect { source: Proxy :: _ "sinOsc", dest: cursorGain }
            lsig <$> (wag l { iteration = iteration + 1 })
    else
      Right (ichange (deltaKsDynamicsCompressor time) $> l)
