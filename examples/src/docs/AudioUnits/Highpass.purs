module Ocarina.Example.Docs.AudioUnits.Highpass where

import Prelude


import Deku.Core (Nut)
import Deku.Pursx (pursx)
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (highpass_, loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)

type Px = """<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
"""

highpass :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
highpass ccb _ ev = pursx @Px
  { highpass:
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [highpass_ 2000.0  [loopBuf buf bangOn]]
      )
  }