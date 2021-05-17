module WAGS.Example.KitchenSink.Types.TriangleOsc where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math (cos, pi, pow, sin, (%))
import Type.Proxy (Proxy(..))
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TRecorder, TTriangleOsc)
import WAGS.Graph.Optionals (CRecorder, CTriangleOsc, DGain, DTriangleOsc, gain_, recorder, triangleOsc, triangleOsc_)

type TriangleOscGraph
  = TopWith { recorder :: Unit }
      ( triangleOsc :: TTriangleOsc /\ {}
      , recorder :: TRecorder "my-recorder" /\ { triangleOsc :: Unit }
      )

ksTriangleOscCreate :: { recorder :: CRecorder "my-recorder" { triangleOsc :: CTriangleOsc } }
ksTriangleOscCreate = { recorder: recorder Proxy { triangleOsc: triangleOsc 440.0 } }

deltaKsTriangleOsc :: Number -> { mix :: DGain, triangleOsc :: DTriangleOsc }
deltaKsTriangleOsc =
  (_ % pieceTime)
    >>> (_ - timing.ksTriangleOsc.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time

          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (0.1 - 0.1 * (cos time))
          , triangleOsc:
              triangleOsc_ (if switchOO then On else Off)
                (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
          }
