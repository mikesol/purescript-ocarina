module WAGS.CheatSheet.CreateScene where

import Prelude

import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit as AU
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, RunEngine, SceneI(..))

type MyGraph
  = ( speaker :: AU.TSpeaker /\ { gain :: Unit }
    , gain :: AU.TGain /\ { osc :: Unit }
    , osc :: AU.TSinOsc /\ {}
    )

initialFrame :: IxWAG () RunAudio RunEngine Frame0 Unit {} { | MyGraph } Unit
initialFrame = ipatch

piece :: Scene (SceneI Unit Unit) () RunAudio RunEngine Frame0 Unit
piece =
  (const initialFrame)
    @!> iloop \(SceneI { time }) _ -> ichange { gain: 0.2, osc: 440.0 + ((time * 15.0) % 30.0) }
