module WAGS.Example.Docs.AudioUnits.Delay where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Core (input, fan)
import WAGS.Control (delay_, gain_, playBuf)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px = Proxy :: Proxy """<section>
  <h2 id="delay">Delay</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DelayNode">Delay</a>, as its name suggests, delays a signal. Using multiple delay nodes, you can create a decent echo effect.</p>

  <p>To create an even <i>better</i> echo effect, you can used fixed points, which is covered in the <a>Fix and fan</a> section of this documentation.</p>

  <pre><code>\buf -> run2_
  [ fan (playBuf buf pureOn)
      \b -> gain_ 0.2
        [ delay_ 0.03 [ input b ]
        , delay_ 0.1 [ input b ]
        , delay_ 0.3 [ input b ]
        , delay_ 0.7 [ input b ]
        ]
  ]</code></pre>

  ~delay~
  </section>
"""

delay :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
delay ccb _ ev = px ~~
  { delay: nut
      ( audioWrapper ev ccb (ctxAff >>= \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/339/339822_5121236-lq.mp3")
          \buf -> run2_
            [ fan (playBuf buf pureOn)
                \b -> gain_ 0.2
                  [ delay_ 0.03 [ input b ]
                  , delay_ 0.1 [ input b ]
                  , delay_ 0.3 [ input b ]
                  , delay_ 0.7 [ input b ]
                  ]
            ]
      )
  }