module WAGS.Example.KitchenSink.Types.TriangleOsc where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math (cos, pi, pow, sin, (%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CRecorder, CTriangleOsc, recorder, triangleOsc)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TRecorder, TTriangleOsc)

type TriangleOscGraph
  = TopWith { recorder :: Unit }
      ( triangleOsc :: TTriangleOsc /\ {}
      , recorder :: TRecorder "my-recorder" /\ { triangleOsc :: Unit }
      )

ksTriangleOscCreate :: { recorder :: CRecorder "my-recorder" { triangleOsc :: CTriangleOsc } }
ksTriangleOscCreate = { recorder: recorder Proxy { triangleOsc: triangleOsc 440.0 } }

deltaKsTriangleOsc :: forall proof. Number -> IxWAGSig' TriangleOscGraph TriangleOscGraph proof Unit
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
          ichange
            { mix: 0.1 - 0.1 * (cos time)
            , triangleOsc:
                { onOff: if switchOO then On else Off
                , freq: 440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0)
                }
            }
