module WAGS.Example.KitchenSink.TLP.SawtoothOsc where

import Prelude

import Control.Applicative.Indexed (ipure, (:*>))
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Control.Functions (ibranch, icont)
import WAGS.Example.KitchenSink.TLP.Convolver (doConvolver)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (SawtoothOscGraph)
import WAGS.Graph.Parameter (_on, cancel)
import WAGS.Patch (ipatch)
import WAGS.Run (BehavingScene(..))

doSawtoothOsc :: forall proof. StepSig SawtoothOscGraph proof
doSawtoothOsc =
  ibranch \(BehavingScene { time, world: { buffers: { "my-buffer": myBuffer, reverb } } }) lsig ->
    if time % pieceTime < timing.ksSawtoothOsc.end then
      Right Ix.do
        -- tests cancel
        when (time % pieceTime > timing.ksSawtoothOsc.begin + 2.0)
          (ichange { sawtoothOsc: cancel })
        ipure lsig
    else
      Left
        $ icont doConvolver (ipatch { microphone: Nothing, mediaElement: Nothing } :*> ichange { buf: { buffer: myBuffer, onOff: _on }, convolver: reverb } $> lsig)
