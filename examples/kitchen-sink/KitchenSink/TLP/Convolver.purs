module WAGS.Example.KitchenSink.TLP.Convolver where

import Prelude

import Control.Applicative.Indexed (ipure, (:*>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Control.Functions (ibranch, icont)
import WAGS.Example.KitchenSink.TLP.Allpass (doAllpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (ksAllpassCreate)
import WAGS.Example.KitchenSink.Types.Convolver (ConvolverGraph)
import WAGS.Patch (ipatch)
import WAGS.Run (BehavingScene(..))

doConvolver :: forall proof. StepSig ConvolverGraph proof
doConvolver =
  ibranch \(BehavingScene { time, world }) lsig ->
    if time % pieceTime < timing.ksConvolver.end then
      Right (ipure lsig)
    else
      Left
        $ icont doAllpass (ipatch { microphone: Nothing, mediaElement: Nothing, subgraphs: {}, tumults: {} } :*> ichange (ksAllpassCreate world) $> lsig)
