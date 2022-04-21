module WAGS.Example.Docs.AudioUnits.SinOsc where

import Prelude

import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, sinOsc)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Parameter (bangOn)
import WAGS.Run (run2)

px =  Proxy   :: Proxy    """<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
"""

sine
  :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
sine ccb _ ev = px ~~
  { periodic: nut
      ( audioWrapper ev ccb (\_ -> pure unit)
          \ctx _ -> run2 ctx
            [gain_ 0.2
            [sinOsc 448.0 bangOn]]
      )
  }