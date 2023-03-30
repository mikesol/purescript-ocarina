module Ocarina.Example.Docs.FixFan.Fan1 where

import Prelude

import Bolson.Core (envy)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Core (Nut)
import Deku.Pursx (makePursx')
import Effect (Effect)
import FRP.Event (Event)
import Ocarina.Control (bandpass_, loopBuf, gain_)
import Ocarina.Control as C
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

px =
  Proxy    :: Proxy         """<div>
  <pre><code>\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
      \b _ -> gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
"""

fan1 :: CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Nut
fan1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0:
      (audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ C.fan1 (loopBuf buf bangOn)
                \b -> gain_ 0.8
                  $ 0 .. 40 <#> lcmap toNumber
                      \i -> bandpass_
                        { frequency: 200.0 + i * 150.0, q: 30.0 }
                        [ b ]
            ]
      )
  }
