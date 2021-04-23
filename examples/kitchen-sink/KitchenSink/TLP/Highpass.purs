module WAGS.Example.KitchenSink.TLP.Highpass where

import Prelude
import Data.Either (Either(..))
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.TLP.Microphone (doMicrophone)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (reset)
import WAGS.Example.KitchenSink.Types.Highpass (HighpassUniverse, ksHighpassHighpass, ksHighpassGain, ksHighpassPlaybuf, deltaKsHighpass)
import WAGS.Graph.Optionals (microphone)

doHighpass :: forall proof iu cb. StepSig (HighpassUniverse cb) proof iu
doHighpass =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksHighpass.end then
          Right (change (deltaKsHighpass time) $> lsig)
        else
          Left
            $ inSitu doMicrophone WAGS.do
                cursorHighpass <- cursor ksHighpassHighpass
                cursorHighpassBuf <- cursor ksHighpassPlaybuf
                cursorGain <- cursor ksHighpassGain
                disconnect cursorHighpassBuf cursorHighpass
                disconnect cursorHighpass cursorGain
                destroy cursorHighpass
                destroy cursorHighpassBuf
                reset
                toAdd <- create microphone
                connect toAdd cursorGain
                withProof pr lsig
