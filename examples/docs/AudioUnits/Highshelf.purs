module WAGS.Example.Docs.AudioUnits.Highshelf where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (highshelf_, loopBuf)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px = Proxy :: Proxy """<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\buf -> run2_
  $ highshelf_ { frequency: 2000.0, gain: 0. }
  $ loopBuf buf pureOn
</code></pre>

  ~highshelf~
  </section>
"""

highshelf :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
highshelf ccb _ ev = px ~~
  { highshelf: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \buf -> run2_
            $ highshelf_ {frequency: 2000.0, gain: 0.4 } $ loopBuf buf pureOn
      )
  }