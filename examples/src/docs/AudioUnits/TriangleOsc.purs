module Ocarina.Example.Docs.AudioUnits.TriangleOsc where

import Prelude


import Deku.Core (Nut)
import Effect (Effect)
import FRP.Poll (Poll)
import Deku.Pursx (pursx)
import Ocarina.Control (gain_, triangleOsc)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Run (run2)

type Px =    """<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
"""

triangle
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
triangle ccb _ ev = pursx @Px
  { periodic:
      ( audioWrapper ev ccb (\_ -> pure unit)
          \ctx _ -> run2 ctx
  [gain_ 0.2
      [triangleOsc 448.0 bangOn]]

      )
  }