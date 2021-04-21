module WAGS.Example.KitchenSink.Piece where

import Prelude

import Data.Functor.Indexed (ivoid)
import Type.Data.Peano as N
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (start, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame0)
import WAGS.Create (create)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..), SceneSig)
import WAGS.Example.KitchenSink.TLP.SinOsc (doSinOsc)
import WAGS.Example.KitchenSink.Types.SinOsc (ksSinOsc)
import WAGS.MoveNode (moveNode)

piece :: SceneSig Frame0
piece =
  WAGS.do
    start
    ivoid $ create ksSinOsc
    moveNode (Proxy :: _ N.D2) (Proxy :: _ N.D0) $> LoopSig doSinOsc
    @|> doSinOsc
