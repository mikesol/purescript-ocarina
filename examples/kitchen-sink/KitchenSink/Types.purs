module WAGS.Example.KitchenSink.Types where

import Prelude
import WAGS.Example.KitchenSink.Types.PeriodicOsc (phase4Time)
import WAGS.Example.KitchenSink.Types.SawtoothOsc (phase5Time)
import WAGS.Example.KitchenSink.Types.SinOsc (SinOscUniverse, phase1Time)
import WAGS.Example.KitchenSink.Types.SquareOsc (phase3Time)
import WAGS.Example.KitchenSink.Types.TriangleOsc (phase2Time)

type BaseUniverse cb
  = SinOscUniverse cb

pieceTime :: Number
pieceTime =
  phase1Time
    + phase2Time
    + phase3Time
    + phase4Time
    + phase5Time
