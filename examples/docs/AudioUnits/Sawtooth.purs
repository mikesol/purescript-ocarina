module Ocarina.Example.Docs.AudioUnits.SawtoothOsc where

import Prelude

import Bolson.Core (envy)
import Deku.Core (Domable)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (AnEvent, Event)
import Hyrule.Zora (Zora)
import Ocarina.Control (gain_, sawtoothOsc)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

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
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> AnEvent Zora SingleSubgraphEvent -> Domable lock payload
sawtooth ccb _ ev = px ~~
  { periodic: nut
      ( audioWrapper ev ccb (\_ -> pure unit)
          \ctx _ -> run2 ctx
  [gain_ 0.2
      [sawtoothOsc 448.0 bangOn]]
      )
  }