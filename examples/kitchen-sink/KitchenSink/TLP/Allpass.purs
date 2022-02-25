module WAGS.Example.KitchenSink.TLP.Allpass where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Connect (iconnect)
import WAGS.Control.Functions (ibranch, icont)
import WAGS.Create (icreate)
import WAGS.Destroy (idestroy)
import WAGS.Disconnect (idisconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.Lowpass (doLowpass)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Allpass (AllpassGraph, deltaKsAllpass)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Lowpass (ksLowpassCreate)
import WAGS.Patch (ipatch)
import WAGS.Run (BehavingScene(..))

doAllpass :: forall proof. StepSig AllpassGraph proof
doAllpass =
  ibranch \(BehavingScene { time, world: world@{ buffers: { "my-buffer": myBuffer } } }) lsig ->
    if time % pieceTime < timing.ksAllpass.end then
      Right (deltaKsAllpass world time $> lsig)
    else if lsig.iteration `mod` 2 == 0 then
      Left
        $ icont doLowpass Ix.do
          let
            cursorAllpass = Proxy :: _ "allpass"
            cursorPlayBuf = Proxy :: _ "buf"
          idisconnect { source: cursorPlayBuf, dest: cursorAllpass }
          idisconnect { source: cursorAllpass, dest: cursorGain }
          idestroy cursorAllpass
          idestroy cursorPlayBuf
          icreate (ksLowpassCreate world)
          iconnect { source: Proxy :: _ "lowpass", dest: cursorGain }
          ipure lsig
    else
      Left
        $ icont doLowpass Ix.do
          ipatch { microphone: Nothing, mediaElement: Nothing }
          ichange
            { lowpass: 300.0
            , buf: myBuffer
            }
          ipure lsig
