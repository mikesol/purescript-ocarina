module WAGS.Example.KitchenSink.TLP.Peaking where

import Prelude
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.Highpass (doHighpass)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (cursorGain)
import WAGS.Example.KitchenSink.Types.Highpass (ksHighpassCreate)
import WAGS.Example.KitchenSink.Types.Peaking (PeakingGraph, deltaKsPeaking)

doPeaking :: forall proof. StepSig PeakingGraph proof
doPeaking =
  branch \lsig -> WAGS.do
    { time } <- env
    ivoid $ modifyRes (const $ "Using a peaking filter")
    pr <- proof
    withProof pr
      $ if time % pieceTime < timing.ksPeaking.end then
          Right (change (deltaKsPeaking time) $> lsig)
        else
          Left
            $ inSitu doHighpass WAGS.do
                let
                  cursorPeaking = Proxy :: _ "peaking"

                  cursorPlayBuf = Proxy :: _ "buf"
                disconnect cursorPlayBuf cursorPeaking
                disconnect cursorPeaking cursorGain
                destroy cursorPeaking
                destroy cursorPlayBuf
                create ksHighpassCreate
                connect (Proxy :: _ "highpass") cursorGain
                withProof pr lsig
