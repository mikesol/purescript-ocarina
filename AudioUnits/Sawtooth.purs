module Ocarina.Example.Docs.AudioUnits.SawtoothOsc where

import Prelude

import Deku.Core (Domable)
import Bolson.Core (envy)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import Ocarina.Control (gain_, sawtoothOsc)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Run (run2)

px =
  Proxy    :: Proxy         """<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
"""

sawtooth
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
sawtooth ccb _ ev = px ~~
  { periodic: nut
      ( envy $ audioWrapper ev ccb (\_ -> pure unit)
          \ctx _ -> run2 ctx
  [gain_ 0.2
      [sawtoothOsc 448.0 bangOn]]
      )
  }