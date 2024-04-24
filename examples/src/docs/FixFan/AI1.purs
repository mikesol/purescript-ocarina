module Ocarina.Example.Docs.FixFan.AI1 where

import Prelude


import Control.Parallel (parallel, sequential)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Number (pow)
import Deku.Core (Nut)
import Deku.Pursx (pursx')
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (apOn, dt)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Properties (onOff)
import Ocarina.Run (run2)

type Px =      """<div>
  <pre><code>\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = pure $ onOff $ dt (add n) apOn
            mtk i =
              case i `mod` 4 of
                0 -> tink0
                1 -> tink1
                2 -> tink2
                _ -> tink3
          0 .. 100 &lt;#&gt;
            \i' -> do
              let i = toNumber i'
              playBuf (mtk i')
                (ooo (0.3 + 0.3 * (i * (1.005 `pow` i))))</code></pre>

  @ai0@
  </div>
"""

ai1 :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
ai1 ccb _ ev = pursx' @"@" @Px
  { ai0:
      ( audioWrapper ev ccb
          ( \ctx -> sequential $ { tink0: _, tink1: _, tink2: _, tink3: _ }
              <$> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/562/562008_7107243-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/126/126531_2044671-lq.mp3")
          )
          \ctx { tink0, tink1, tink2, tink3 } -> run2 ctx
            [ gain_ 1.0
                $ do
                    let
                      ooo n = pure $ onOff $ dt (add n) apOn
                      mtk i =
                        case i `mod` 4 of
                          0 -> tink0
                          1 -> tink1
                          2 -> tink2
                          _ -> tink3
                    0 .. 100 <#>
                      \i' -> do
                        let i = toNumber i'
                        playBuf (mtk i')
                          (ooo (0.3 + 0.3 * (i * (1.005 `pow` i))))
            ]
      )
  }
