module WAGS.Example.Docs.AudioUnits.TriangleOsc where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, triangleOsc)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px =
  Proxy    :: Proxy         """<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\buf -> run2_
  $ gain_ 0.2
      $ triangleOsc 448.0 pureOn
</code></pre>

  ~periodic~
  </section>
"""

triangle
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
triangle ccb _ ev = px ~~
  { periodic: nut
      ( audioWrapper ev ccb (pure unit)
          \_ -> run2_
  $ gain_ 0.2
      $ triangleOsc 448.0 pureOn

      )
  }