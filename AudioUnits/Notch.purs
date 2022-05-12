module WAGS.Example.Docs.AudioUnits.Notch where

import Prelude

import Deku.Core (Domable, envy)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf, notch_)
import WAGS.Core (bangOn)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<section>
<h2 id="notch">Notch filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">notch filter</a>, also known as a band-reject filter, attenuates a single frequency range of a source. When you crank up their Q value, the attenuation gets more intense. At the extreme, it sounds like part of the source got sucked into a vacuum, which is not un-interesting!</p>

  <pre><code>\buf -> run2_
  [
    notch_ { frequency: 400.0, q: 1.0 }
    $ pure $ notch_ { frequency: 880.0, q: 5.0 }
    $ pure $ notch_ { frequency: 1200.0, q: 10.0 }
    $ pure $ notch_ { frequency: 2000.0, q: 20.0 }
    $ pure $ notch_ { frequency: 3000.0, q: 30.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~notch~
  </section>
"""

notch :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
notch ccb _ ev = px ~~
  { notch: nut
      ( envy $ audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [
              notch_ { frequency: 400.0, q: 1.0 }
              $ pure $ notch_ { frequency: 880.0, q: 5.0 }
              $ pure $ notch_ { frequency: 1200.0, q: 10.0 }
              $ pure $ notch_ { frequency: 2000.0, q: 20.0 }
              $ pure $ notch_ { frequency: 3000.0, q: 30.0 }
              $ pure $ loopBuf buf bangOn
            ]
      )
  }