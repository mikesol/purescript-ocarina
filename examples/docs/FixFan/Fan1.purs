module WAGS.Example.Docs.FixFan.Fan1 where

import Prelude

import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (bandpass_, loopBuf, gain_)
import WAGS.Control as C
import WAGS.Core (mix)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Core (bangOn)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<div>
  <pre><code>\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
      \b _ -> mix $ gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
"""

fan1 :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
fan1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0: nut
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ C.fan1 (loopBuf buf bangOn)
                \b _ -> mix $ gain_ 0.8
                  $ 0 .. 40 <#> lcmap toNumber
                      \i -> bandpass_
                        { frequency: 200.0 + i * 150.0, q: 30.0 }
                        [ b ]
            ]
      )
  }
