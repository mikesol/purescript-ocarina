module WAGS.Example.Docs.AudioUnits.SinOsc where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, sinOsc)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px =  Proxy   :: Proxy    """<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\buf -> run2_
  $ gain_ 0.2 $ sinOsc 448.0 pureOn
</code></pre>

  ~periodic~
  </section>
"""

sine
  :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
sine ccb _ ev = px ~~
  { periodic: nut
      ( audioWrapper ev ccb (pure unit)
          \_ -> run2_
            $ gain_ 0.2
            $ sinOsc 448.0 pureOn
      )
  }