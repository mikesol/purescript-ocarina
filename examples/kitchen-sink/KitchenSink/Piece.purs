module WAGS.Example.KitchenSink.Piece where

import Prelude
import WAGS.Control.Functions ((@!>))
import WAGS.Control.Types (Frame0)
import WAGS.Create (icreate)
import WAGS.Example.KitchenSink.TLP.LoopSig (LoopSig(..), SceneSig)
import WAGS.Example.KitchenSink.TLP.SinOsc (doSinOsc)
import WAGS.Example.KitchenSink.Types.SinOsc (ksSinOscCreate)
import WAGS.Graph.Optionals (gain, speaker)

piece :: SceneSig Frame0
piece =
  ( \_ ->
      (icreate $ speaker { mix: gain 0.0 ksSinOscCreate })
        $> { loop: LoopSig doSinOsc, iteration: 0 }
  )
    @!> doSinOsc
