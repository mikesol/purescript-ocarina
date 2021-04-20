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

ksSinOscTime = 5.0 :: Number

ksSinOscIntegral = ksSinOscTime :: Number

ksTriangleOscTime = 5.0 :: Number

ksTriangleOscIntegral = ksTriangleOscTime + ksSinOscIntegral :: Number

ksSquareOscTime = 5.0 :: Number

ksSquareOscIntegral = ksSquareOscTime + ksTriangleOscIntegral :: Number

ksPeriodicOscTime = 5.0 :: Number

ksPeriodicOscIntegral = ksPeriodicOscTime + ksSquareOscIntegral :: Number

ksSawtoothOscTime = 5.0 :: Number

ksSawtoothOscIntegral = ksSawtoothOscTime + ksPeriodicOscIntegral :: Number

ksAllpassTime = 10.0 :: Number

ksAllpassIntegral = ksAllpassTime + ksSawtoothOscIntegral :: Number

ksLowpassTime = 10.0 :: Number

ksLowpassIntegral = ksLowpassTime + ksAllpassIntegral :: Number

ksHighshelfTime = 10.0 :: Number

ksHighshelfIntegral = ksHighshelfTime + ksLowpassIntegral :: Number

ksLowshelfTime = 10.0 :: Number

ksLowshelfIntegral = ksLowshelfTime + ksHighshelfIntegral :: Number

ksBandpassTime = 10.0 :: Number

ksBandpassIntegral = ksBandpassTime + ksLowshelfIntegral :: Number

ksNotchTime = 10.0 :: Number

ksNotchIntegral = ksNotchTime + ksBandpassIntegral :: Number

ksPeakingTime = 10.0 :: Number

ksPeakingIntegral = ksPeakingTime + ksNotchIntegral :: Number

ksHighpassTime = 10.0 :: Number

ksHighpassIntegral = ksHighpassTime + ksPeakingIntegral :: Number

pieceTime :: Number
pieceTime = ksAllpassIntegral
