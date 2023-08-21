module Ocarina.Example.Docs.FixFan.AI0 where

import Prelude


import Control.Parallel (parallel, sequential)
import Deku.Core (Nut)
import Deku.Pursx (makePursx')
import Effect (Effect)
import FRP.Poll (Poll)
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
  [ gain_ 1.0 do
      let ooo n = pure $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
"""

ai0 :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
ai0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0:
      ( audioWrapper ev ccb
          ( \ctx -> sequential $ { tink0: _, tink1: _, tink2: _, tink3: _ }
              <$> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/562/562008_7107243-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/126/126531_2044671-lq.mp3")
          )
          \ctx { tink0, tink1, tink2, tink3 } -> run2 ctx
            [ gain_ 1.0 do
                let ooo n = pure $ onOff $ dt (add n) apOn
                [ playBuf tink0 (ooo 0.1)
                , playBuf tink1 (ooo 0.2)
                , playBuf tink2 (ooo 0.9)
                , playBuf tink3 (ooo 1.8)
                ]
            ]
      )
  }
