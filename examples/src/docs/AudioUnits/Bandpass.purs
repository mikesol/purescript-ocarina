module Ocarina.Example.Docs.AudioUnits.Bandpass where

import Prelude

import Deku.Core (Nut)
import Deku.Pursx (makePursx')
import Effect (Effect)
import FRP.Event (Event)
import Ocarina.Control (bandpass_, fan1, gain_, loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

px =
  Proxy    :: Proxy         """<section>
  <h2 id="bandpass">Bandpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">bandpass filter</a> isolates a single frequency range of a source. When you crank up a bandpass node's Q value, the isolation gets more intense. At the extreme, the source signal is almost lost and you get a pure sound that resembles a sine-wave oscillator.</p>

  <pre><code>\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \b _ -> gain_ 0.8
      [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
      , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
      , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
      , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
      , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
      ]
  ]</code></pre>

  @bandpass@
  </section>
"""

bandpass :: CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Nut
bandpass ccb _ ev = makePursx' (Proxy :: _ "@") px
  { bandpass:
      (  audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ fan1 (loopBuf buf bangOn)
                \b -> gain_ 0.8
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