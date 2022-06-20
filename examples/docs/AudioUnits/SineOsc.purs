module Ocarina.Example.Docs.AudioUnits.SinOsc where

import Prelude

import Deku.Core (Domable, envy)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import Ocarina.Control (gain_, sinOsc)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Run (run2)

px =  Proxy   :: Proxy    """<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
"""

sine
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
sine ccb _ ev = px ~~
  { periodic: nut
      ( envy $ audioWrapper ev ccb (\_ -> pure unit)
          \ctx _ -> run2 ctx
            [gain_ 0.2
            [sinOsc 448.0 bangOn]]
      )
  }