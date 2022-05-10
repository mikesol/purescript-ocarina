module WAGS.Example.Docs.FixFan.Fix0 where

import Prelude

import Deku.Core (Domable, toDOM)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (delay_, gain_, playBuf, fix)
import WAGS.Core (bangOn)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<div>
  <pre><code>\buf -> run2_
  [ fix
      \b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
"""

fix0 :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
fix0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0: nut
      (toDOM $ audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
          \ctx buf -> run2 ctx
            [ fix
                \b -> gain_ 1.0
                  [ playBuf buf bangOn
                  , delay_ 0.1 [ gain_ 0.6 [ b ] ]
                  ]
            ]
      )
  }
