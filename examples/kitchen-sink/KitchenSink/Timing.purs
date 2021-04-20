module WAGS.Example.KitchenSink.Timing where

import Prelude

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

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

phase6Time = 10.0 :: Number

phase6Integral = phase6Time + phase5Integral :: Number

phase7Time = 10.0 :: Number

phase7Integral = phase7Time + phase6Integral :: Number

pieceTime :: Number
pieceTime = phase6Integral
