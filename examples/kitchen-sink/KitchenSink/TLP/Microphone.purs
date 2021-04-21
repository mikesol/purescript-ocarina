module WAGS.Example.KitchenSink.TLP.Microphone where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Effect (Effect)
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame, Scene)
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.TLP.WaveShaper (doWaveShaper)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Microphone (MicrophoneUniverse, ksMicrophoneMicrophone, ksMicrophoneGain, deltaKsMicrophone)
import WAGS.Example.KitchenSink.Types.WaveShaper (ksWaveShaperCreate)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doMicrophone ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (MicrophoneUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doMicrophone =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksMicrophoneMicrophone
    gn <- cursor ksMicrophoneGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksMicrophone.end then
          Right (change (deltaKsMicrophone time) $> lsig)
        else
          Left
            $ inSitu doWaveShaper WAGS.do
                disconnect toRemove gn
                destroy toRemove
                reset
                toAdd <- create (ksWaveShaperCreate Identity Identity)
                connect toAdd gn
                withProof pr lsig
