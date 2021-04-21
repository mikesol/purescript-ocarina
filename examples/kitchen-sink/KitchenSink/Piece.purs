module WAGS.Example.KitchenSink.Piece where

import Prelude

import Data.Functor.Indexed (ivoid)
import Effect (Effect)
import Type.Data.Peano as N
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions ( start, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (create)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..))
import WAGS.Example.KitchenSink.TLP.SinOsc (doSinOsc)
import WAGS.Example.KitchenSink.Types.SinOsc (ksSinOsc)
import WAGS.Interpret (FFIAudio)
import WAGS.MoveNode (moveNode)
import WAGS.Run (SceneI)

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
piece =
  WAGS.do
    start
    ivoid $ create ksSinOsc
    moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0) $> LoopSig doSinOsc
    @|> doSinOsc
