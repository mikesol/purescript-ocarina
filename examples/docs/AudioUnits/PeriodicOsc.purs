module WAGS.Example.Docs.AudioUnits.PeriodicOsc where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Vec (empty, (+>))
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, periodicOsc)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Parameter (bangOn)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<section>
  <h2 id="periodic">Periodic wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">periodic wave oscillator</a> plays back a custom periodic waveform at a given frequency. The custom waveform must be set as part of the initialization and can be changed after initialization. Note that the change will not go into effect if the oscillator is on: it must be turned off and on again.</p>

  <pre><code>\buf -> run2_
  [
    gain_ 0.2
      [
        periodicOsc
          { frequency: 140.0
          , spec:
              ( (0.1 +> 0.2 +> 0.3 +> 0.4 +> empty)
                  /\ (0.4 +> 0.3 +> 0.2 +> 0.1 +> empty)
              )
          }
          bangOn
      ]
  ]</code></pre>

  ~periodic~
  </section>
"""

periodic
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
periodic ccb _ ev = px ~~
  { periodic: nut
      ( audioWrapper ev ccb (\_ -> pure unit)
          \ctx _ -> run2 ctx
            [gain_ 0.2
            [periodicOsc
                { frequency: 140.0
                , spec:
                    ( (0.1 +> 0.2 +> 0.3 +> 0.4 +> empty)
                        /\ (0.4 +> 0.3 +> 0.2 +> 0.1 +> empty)
                    )
                }
                bangOn]]

      )
  }