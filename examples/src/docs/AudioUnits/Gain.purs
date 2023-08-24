module Ocarina.Example.Docs.AudioUnits.Gain where

import Prelude


import Deku.Core (Nut)
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (gain_, loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

px = Proxy :: Proxy """<section>
  <h2 id="gain">Gain</h2>
  <p>The almighty <a href="https://developer.mozilla.org/en-US/docs/Web/API/GainNode">gain</a> node is your friendly neighborhood volume control. Volume in the web-audio API goes from 0 to 1 whereas we hear logarithmically, so when you're using this, make sure to convert between decibels and gain if you want to work with more intuitive units. The conversion formula is as follows:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>And here's a little example of a single gain node:</p>

  <pre><code>run2_
  [ gain_ 0.1
    [ loopBuf buf bangOn ]
  ] </code></pre>

  ~gain~
  </section>
"""

gain :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
gain ccb _ ev = px ~~
  { gain:
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/339/339822_5121236-lq.mp3")
          \ctx buf -> run2 ctx
            [ gain_ 0.1  [ loopBuf buf bangOn ]]
      )
  }