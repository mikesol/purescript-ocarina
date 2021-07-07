module WAGS.Example.KitchenSink.TLP.Constant where

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
import WAGS.Example.KitchenSink.TLP.DynamicsCompressor (doDynamicsCompressor)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Constant (ConstantGraph, deltaKsConstant)
import WAGS.Example.KitchenSink.Types.DynamicsCompressor (ksDynamicsCompressorCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Run (SceneI(..))

doConstant :: forall proof. StepSig ConstantGraph proof
doConstant =
  ibranch \(SceneI { time }) lsig ->
    if time % pieceTime < timing.ksConstant.end then
      Right (deltaKsConstant time $> lsig)
    else
      Left
        $ icont doDynamicsCompressor Ix.do
            let
              cursorConstant = Proxy :: _ "constant"
            idisconnect { source: cursorConstant, dest: cursorGain }
            idestroy cursorConstant
            icreate ksDynamicsCompressorCreate
            iconnect { source: Proxy :: _ "compressor", dest: cursorGain }
            ipure lsig
