module Ocarina.Example.Docs.FixFan.AI1 where

import Prelude

import Bolson.Core (envy)
import Control.Parallel (parallel, sequential)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Number (pow)
import Deku.Core (Domable)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (AnEvent, Event)
import Hyrule.Zora (Zora)
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (apOn, dt)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Properties (onOff)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

px =
  Proxy    :: Proxy         """<div>
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

ai1 :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> AnEvent Zora SingleSubgraphEvent -> Domable lock payload
ai1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0: nut
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
