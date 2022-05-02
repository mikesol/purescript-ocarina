module WAGS.Example.Docs.AudioUnits.Peaking where

import Prelude

import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf, peaking_)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Core (bangOn)
import WAGS.Run (run2)

px =
  Proxy :: Proxy """<section>
  <h2 id="peaking">Peaking filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">peaking filter</a> is sort of like a notch/bandpass combo. It sounds different than bandpass or notch, and is often a better choice depending on what you're making. The Q works as normal, but the gain either boosts or attenuates the frequency in question if it is positive or negative.</p>

  <pre><code>\buf -> run2_
  [
    peaking_ { frequency: 400.0, q: 1.0, gain: -20.0 }
    $ peaking_ { frequency: 880.0, q: 5.0, gain: 20.0 }
    $ peaking_ { frequency: 1200.0, q: 10.0, gain: -20.0 }
    $ peaking_ { frequency: 2000.0, q: 20.0, gain: 20.0 }
    $ peaking_ { frequency: 3000.0, q: 30.0, gain: -20.0 }
    $ loopBuf buf bangOn
  ]</code></pre>

  ~peaking~
  </section>
"""

peaking
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
peaking ccb _ ev = px ~~
  { peaking: nut
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [
              peaking_ { frequency: 400.0, q: 1.0, gain: -20.0 }
              $ peaking_ { frequency: 880.0, q: 5.0, gain: 20.0 }
              $ peaking_ { frequency: 1200.0, q: 10.0, gain: -20.0 }
              $ peaking_ { frequency: 2000.0, q: 20.0, gain: 20.0 }
              $ peaking_ { frequency: 3000.0, q: 30.0, gain: -20.0 }
              $ loopBuf buf bangOn
            ]
      )
  }