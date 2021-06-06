module WAGS.Example.KitchenSink.Types.DynamicsCompressor where

import Prelude
import Data.Tuple.Nested (type (/\))
import Math ((%))
import WAGS.Example.KitchenSink.Timing (timing, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (OnOff(..), TDynamicsCompressor, TPlayBuf)
import WAGS.Create.Optionals (CDynamicsCompressor, CPlayBuf, compressor, playBuf)

type DynamicsCompressorGraph
  = TopWith { compressor :: Unit }
      ( compressor :: TDynamicsCompressor /\ { buf :: Unit }
      , buf :: TPlayBuf /\ {}
      )

ksDynamicsCompressorCreate :: { compressor :: CDynamicsCompressor { buf :: CPlayBuf } }
ksDynamicsCompressorCreate = { compressor: compressor {} { buf: playBuf "my-buffer" } }

deltaKsDynamicsCompressor :: Number -> ?hole -- { mix :: DGain, compressor :: DDynamicsCompressor, buf :: DPlayBuf }
deltaKsDynamicsCompressor =
  (_ % pieceTime)
    >>> (_ - timing.ksDynamicsCompressor.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          switchOO = time % 2.0 < 1.0

          switchW = time % 4.0 < 2.0
        in
          { mix: gain_ (if time > (timing.ksDynamicsCompressor.dur - 1.0) then 0.0 else 1.0)
          , compressor:
              compressor_
                { threshold: if time > (dur / 2.0) then -50.0 else -40.0
                , knee: if time > (dur / 3.0) then 20.0 else 40.0
                , ratio: if time > (dur / 4.0) then 2.0 else 5.0
                , attack: if time > (dur / 5.0) then 0.003 else 0.005
                , release: if time > (dur / 6.0) then 0.25 else 0.5
                }
          , buf:
              playBuf_
                { onOff: if switchOO then On else Off }
                (if switchW then "my-buffer" else "shruti")
          }
  where
  dur = timing.ksDynamicsCompressor.dur
