module Ocarina.Example.Docs.AudioUnits.Allpass where

import Prelude

import Deku.Core (Nut)
import Deku.Pursx (pursx')
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (allpass_, fan1, gain_, loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)

type Px =  """<section>
  <h2 id="allpass">Allpass filter</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">all-pass filter</a> <a href="https://en.wikipedia.org/wiki/All-pass_filter">passes through all frequencies of a source at equal volume but changes their phase</a>. Its use by itself is imperceptible, as the human ear (mostly) does not pick up on phase shifts by themselves. However, when an all-pass filter's output is mixed with several chained all-pass filters plus the original source, you hear a neat phaser effect.</p>

  <p>The <code>bangOn</code> is an event that turns the loop buffer on. We'll learn more about turning things on and off in the "Events" section.</p>

  <pre><code>\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \b _ -> gain_ 0.2
      [ b
      , allpass_ 700.0
          [ allpass_ { frequency: 990.0, q: 20.0 } [ b ]
          , allpass_ 1110.0
              [ b
              , allpass_ { frequency: 2010.0, q: 30.0 } [ b ]
              ]
          ]
      ]
  ]
</code></pre>

  @allpass@
  </section>
"""

allpass
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
allpass ccb _ ev = pursx' @"@" @Px
  { allpass:
      (  audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ fan1 (loopBuf buf bangOn)
                \b -> gain_ 0.2
                  [ b
                  , allpass_ 700.0
                      [ allpass_ { frequency: 990.0, q: 20.0 } [ b ]
                      , allpass_ 1110.0
                          [ b
                          , allpass_ { frequency: 2010.0, q: 30.0 } [ b ]
                          ]
                      ]
                  ]
            ]
      )
  }