module WAGS.Example.KitchenSink.TLP.StereoPanner where

import Prelude

import Data.Either (Either(..))
import Math ((%))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.Constant (doConstant)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.StereoPanner (StereoPannerUniverse, deltaKsStereoPanner, ksStereoPannerStereoPanner, ksStereoPannerGain, ksStereoPannerPlaybuf)
import WAGS.Graph.Optionals (constant)


doStereoPanner :: forall proof iu cb. StepSig (StereoPannerUniverse cb) proof iu
doStereoPanner =
  branch \lsig -> WAGS.do
    { time } <- env
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksStereoPanner.end then
          Right (change (deltaKsStereoPanner time) $> lsig)
        else
          Left
            $ inSitu doConstant WAGS.do
                cursorStereoPanner <- cursor ksStereoPannerStereoPanner
                cursorStereoPannerBuf <- cursor ksStereoPannerPlaybuf
                cursorGain <- cursor ksStereoPannerGain
                disconnect cursorStereoPannerBuf cursorStereoPanner
                disconnect cursorStereoPanner cursorGain
                destroy cursorStereoPanner
                destroy cursorStereoPannerBuf
                reset
                toAdd <- create (constant 0.0)
                connect toAdd cursorGain
                withProof pr lsig
