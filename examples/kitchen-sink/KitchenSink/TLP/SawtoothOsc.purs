module WAGS.Example.KitchenSink.TLP.SawtoothOsc where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Control.Plus (empty)
import Data.Either (Either(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, icont)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.Allpass (doAllpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (ksAllpassCreate)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (SawtoothOscGraph)
import WAGS.Graph.Parameter (AudioParameter_)

doSawtoothOsc :: forall proof. StepSig SawtoothOscGraph proof
doSawtoothOsc =
  ibranch \{ time } lsig ->
    if time % pieceTime < timing.ksSawtoothOsc.end then
      Right Ix.do
        -- tests cancel
        when (time % pieceTime > timing.ksSawtoothOsc.begin + 2.0)
          (ichange { sawtoothOsc: empty :: AudioParameter_ Number })
        ipure lsig
    else
      Left
        $ icont doAllpass Ix.do
            let
              cursorSawtoothOsc = Proxy :: _ "sawtoothOsc"
            idisconnect { source: cursorSawtoothOsc, dest: cursorGain }
            idestroy cursorSawtoothOsc
            icreate ksAllpassCreate
            iconnect { source: Proxy :: _ "allpass", dest: cursorGain }
            ipure lsig
