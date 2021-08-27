module WAGS.Example.KitchenSink.TLP.SawtoothOsc where

import Prelude

import Control.Applicative.Indexed (ipure, (:*>))
import Control.Monad.Indexed.Qualified as Ix
import Control.Plus (empty)
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Functions (ibranch, icont)
import WAGS.Example.KitchenSink.TLP.Convolver (doConvolver)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (SawtoothOscGraph)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (AudioParameter_)
import WAGS.Patch (ipatch)
import WAGS.Run (SceneI(..))

doSawtoothOsc :: forall proof. StepSig SawtoothOscGraph proof
doSawtoothOsc =
  ibranch \(SceneI { time }) lsig ->
    if time % pieceTime < timing.ksSawtoothOsc.end then
      Right Ix.do
        -- tests cancel
        when (time % pieceTime > timing.ksSawtoothOsc.begin + 2.0)
          (ichange { sawtoothOsc: empty :: AudioParameter_ Number })
        ipure lsig
    else
      Left
        $ icont doConvolver (ipatch :*> ichange { buf: {buffer: (Proxy :: _ "my-buffer"), onOff: On } } $> lsig)
