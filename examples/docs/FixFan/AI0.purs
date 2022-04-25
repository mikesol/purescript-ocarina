module WAGS.Example.Docs.FixFan.AI0 where

import Prelude

import Control.Parallel (parallel, sequential)
import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, playBuf)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Parameter (apOn, dt)
import WAGS.Properties (onOff)
import WAGS.Run (run2, run2_)

px =
  Proxy    :: Proxy         """<div>
  <pre><code>\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = bang $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
"""

ai0 :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
ai0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0: nut
      ( audioWrapper ev ccb
          ( \ctx -> sequential $ { tink0: _, tink1: _, tink2: _, tink3: _ }
              <$> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/562/562008_7107243-lq.mp3")
              <*> (parallel $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/126/126531_2044671-lq.mp3")
          )
          \ctx { tink0, tink1, tink2, tink3 } -> run2 ctx
            [ gain_ 1.0 do
                let ooo n = bang $ onOff $ dt (add n) apOn
                [ playBuf tink0 (ooo 0.1)
                , playBuf tink1 (ooo 0.2)
                , playBuf tink2 (ooo 0.9)
                , playBuf tink3 (ooo 1.8)
                ]
            ]
      )
  }
