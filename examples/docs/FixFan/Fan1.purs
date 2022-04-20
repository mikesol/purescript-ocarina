module WAGS.Example.Docs.FixFan.Fan1 where

import Prelude

import Control.Plus (class Plus)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut, (~~))
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (bandpass_, loopBuf, gain_)
import WAGS.Core (fan)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Parameter (bangOn)
import WAGS.Run (run2_)

px =
  Proxy    :: Proxy         """<div>
  <pre><code>\buf -> run2_
  [ fan (loopBuf buf bangOn)
      \b -> gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
"""

fan1 :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
fan1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \buf -> run2_
            [ fan (loopBuf buf bangOn)
                \b -> gain_ 0.8
                  $ 0 .. 40 <#> lcmap toNumber
                      \i -> bandpass_
                        { frequency: 200.0 + i * 150.0, q: 30.0 }
                        [ b ]
            ]
      )
  }
