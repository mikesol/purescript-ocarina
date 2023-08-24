module Ocarina.Example.Docs.AudioUnits.Lowpass where

import Prelude


import Deku.Core (Nut)
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (loopBuf, lowpass_)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

px = Proxy :: Proxy """<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
"""

lowpass :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
lowpass ccb _ ev = px ~~
  { lowpass:
      (audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [lowpass_ 215.0 [loopBuf buf bangOn]]
      )
  }