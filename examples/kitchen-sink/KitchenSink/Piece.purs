module WAGS.Example.KitchenSink.Piece where

import Prelude

import WAGS.Control.Functions (start, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame0)
import WAGS.Create (create)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..), SceneSig)
import WAGS.Example.KitchenSink.TLP.SinOsc (doSinOsc)
import WAGS.Example.KitchenSink.Types.SinOsc (ksSinOscCreate)
import WAGS.Graph.Optionals (gain, speaker)

piece :: SceneSig Frame0
piece =
  WAGS.do
    start
    (create $ speaker { mix: gain 1.0 ksSinOscCreate }) $> LoopSig doSinOsc
    @|> doSinOsc
