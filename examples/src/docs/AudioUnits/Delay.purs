module Ocarina.Example.Docs.AudioUnits.Delay where

import Prelude

import Deku.Core (Nut)
import Deku.Pursx (pursx')
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (delay_, fan1, gain_, playBuf)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)

type Px =      """<section>
  <h2 id="delay">Delay</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DelayNode">Delay</a>, as its name suggests, delays a signal. Using multiple delay nodes, you can create a decent echo effect.</p>

  <p>To create an even <i>better</i> echo effect, you can used fixed points, which is covered in the <a>Fix and fan</a> section of this documentation.</p>

  <pre><code>\buf -> run2_
  [ fan1 (playBuf buf bangOn)
      \b _ -> gain_ 0.2
        [ delay_ 0.03 [ b ]
        , delay_ 0.1 [ b ]
        , delay_ 0.3 [ b ]
        , delay_ 0.7 [ b ]
        ]
  ]</code></pre>

  @delay@
  </section>
"""

delay :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
delay ccb _ ev = pursx' @"@" @Px
  { delay:
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/339/339822_5121236-lq.mp3")
          \ctx buf -> run2 ctx
            [ fan1 (playBuf buf bangOn)
                \b -> gain_ 0.2
                  [ delay_ 0.03 [ b ]
                  , delay_ 0.1 [ b ]
                  , delay_ 0.3 [ b ]
                  , delay_ 0.7 [ b ]
                  ]
            ]

      )
  }