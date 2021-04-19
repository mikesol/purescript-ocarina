module WAGS.Example.KitchenSink.Types where

import Prelude

phase1Time = 5.0 :: Number

phase1Integral = phase1Time :: Number

phase2Time = 5.0 :: Number

phase2Integral = phase2Time + phase1Integral :: Number

phase3Time = 5.0 :: Number

phase3Integral = phase3Time + phase2Integral :: Number

phase4Time = 5.0 :: Number

phase4Integral = phase4Time + phase3Integral :: Number

phase5Time = 5.0 :: Number

phase5Integral = phase5Time + phase4Integral :: Number

pieceTime :: Number
pieceTime = phase5Integral
