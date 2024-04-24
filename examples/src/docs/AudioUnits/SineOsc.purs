module Ocarina.Example.Docs.AudioUnits.SinOsc where

import Prelude


import Deku.Core (Nut)
import Deku.Pursx (pursx)
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (gain_, sinOsc)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Run (run2)

type Px =  """<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
"""

sine
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
sine ccb _ ev = pursx @Px
  { periodic:
      ( audioWrapper ev ccb (\_ -> pure unit)
          \ctx _ -> run2 ctx
            [gain_ 0.2
            [sinOsc 448.0 bangOn]]
      )
  }