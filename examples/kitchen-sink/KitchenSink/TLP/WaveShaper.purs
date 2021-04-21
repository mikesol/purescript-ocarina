module WAGS.Example.KitchenSink.TLP.WaveShaper where

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
import WAGS.Example.KitchenSink.TLP.Delay (doDelay)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Delay (ksDelayCreate)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.WaveShaper (WaveShaperUniverse, deltaKsWaveShaper, ksWaveShaperGain, ksWaveShaperPlaybuf, ksWaveShaperWaveShaper)
import WAGS.Interpret (FFIAudio)
import WAGS.Run (SceneI)

doWaveShaper ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (WaveShaperUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doWaveShaper =
  branch \lsig -> WAGS.do
    { time } <- env
    toRemove <- cursor ksWaveShaperWaveShaper
    toRemoveBuf <- cursor ksWaveShaperPlaybuf
    gn <- cursor ksWaveShaperGain
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksWaveShaper.end then
          Right (change (deltaKsWaveShaper time) $> lsig)
        else
          Left
            $ inSitu doDelay WAGS.do
                disconnect toRemoveBuf toRemove
                disconnect toRemove gn
                destroy toRemoveBuf
                destroy toRemove
                reset
                toAdd <- create (ksDelayCreate Identity Identity Identity)
                connect toAdd gn
                withProof pr lsig
