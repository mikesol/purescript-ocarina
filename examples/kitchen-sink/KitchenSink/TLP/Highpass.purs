module WAGS.Example.KitchenSink.TLP.Highpass where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, currentIdx, env, graph, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame, Scene)
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..))
import WAGS.Example.KitchenSink.Timing (phase6Integral, pieceTime)
import WAGS.Example.KitchenSink.Types.Highpass (HighpassUniverse, phase7Highpass, phase7Gain, phase7Playbuf, deltaPhase7)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscGraph)
import WAGS.Graph.Constructors (OnOff(..), SinOsc(..))
import WAGS.Interpret (FFIAudio)
import WAGS.Rebase (rebase)
import WAGS.Run (SceneI)
import WAGS.Universe.Bin (D3)

doHighpass ::
  forall proofA iu cb.
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proofA iu (HighpassUniverse cb) LoopSig ->
  Scene (SceneI Unit Unit) FFIAudio (Effect Unit) proofA
doHighpass =
  branch \l@(LoopSig lsig) -> WAGS.do
    { time } <- env
    toRemove <- cursor phase7Highpass
    toRemoveBuf <- cursor phase7Playbuf
    gn <- cursor phase7Gain
    pr <- proof
    withProof pr
      $ if time % pieceTime < phase6Integral then
          Left \thunk ->
            lsig WAGS.do
              thunk
              toAdd <- create (SinOsc On 440.0)
              disconnect toRemoveBuf toRemove
              disconnect toRemove gn
              connect toAdd gn
              destroy toRemove
              destroy toRemoveBuf
              ci <- currentIdx
              g <- graph
              rebase ci g (Proxy :: _ D3) (Proxy :: _ SinOscGraph)
              withProof pr l
        else
          Right (change (deltaPhase7 time) $> l)
