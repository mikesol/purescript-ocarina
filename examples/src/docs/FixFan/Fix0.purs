module Ocarina.Example.Docs.FixFan.Fix0 where

import Prelude


import Deku.Core (Nut)
import Deku.Pursx (makePursx')
import Effect (Effect)
import FRP.Event (Event)
import FRP.Poll (Poll)
import Ocarina.Control (delay_, gain_, playBuf, fix)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

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

fix0 :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
fix0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0:
      (audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
          \ctx buf -> run2 ctx
            [ fix
                \b -> gain_ 1.0
                  [ playBuf buf bangOn
                  , delay_ 0.1 [ gain_ 0.6 [ b ] ]
                  ]
            ]
      )
  }
