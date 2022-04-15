module WAGS.Example.Docs.AudioUnits.Lowshelf where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf, lowshelf_)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px = Proxy :: Proxy """<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\buf -> run2_
  [ lowshelf_ {frequency: 91.0, gain: 10.0 }
      [loopBuf buf pureOn]
  ]
</code></pre>

  ~lowshelf~
  </section>
"""

lowshelf :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
lowshelf ccb _ ev = px ~~
  { lowshelf: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \buf -> run2_
            [ lowshelf_ {frequency: 91.0, gain: 0.4 } [loopBuf buf pureOn] ]
      )
  }