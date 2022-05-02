module WAGS.Example.Docs.FixFan.Fan0 where

import Prelude

import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (bandpass_, fan1, gain_, loopBuf)
import WAGS.Core (mix)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Core (bangOn)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<div>
  <pre><code>run2_
  [ fan1 (loopBuf buf bangOn)
      \b _ -> mix $ gain_ 0.8
        [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
        , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
        , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
        , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
        , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
        ]
  ]</code></pre>

  @ai0@
  </div>
"""

fan0 :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
fan0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0: nut
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ fan1 (loopBuf buf bangOn)
                \b _ -> mix $ gain_ 0.8
                  [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
                  , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
                  , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
                  , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
                  , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
                  ]
            ]

      )
  }

{-
fan (loopBuf buf bangOn)
  \b -> gain_ 0.8
    ( bandpass_ { frequency: 400.0, q: 1.0 } [b]
    ~ bandpass_ { frequency: 880.0, q: 5.0 } [b]
    ~ bandpass_ { frequency: 1200.0, q: 10.0 } [b]
    ~ bandpass_ { frequency: 2000.0, q: 20.0 } [b]
    ! bandpass_ { frequency: 3000.0, q: 30.0 } [b]
    )


  -}