module Ocarina.Math where

import Prelude

import Data.Number (pow)

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

calcSlopeExp
  :: Number -> Number -> Number -> Number -> Number -> Number -> Number
calcSlopeExp x0 y0 x1 y1 exp x' =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      dx = x1 - x0

      x = ((((x' - x0) / dx) `pow` exp) * dx) + x0

      m = (y1 - y0) / dx

      b = y0 - m * x0
    in
      m * x + b