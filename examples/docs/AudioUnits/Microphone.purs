module WAGS.Example.Docs.AudioUnits.Microphone where

import Prelude

import Data.Maybe (Maybe(..))
import Deku.Core (Domable, Element, toDOM)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (delay_, fix, gain_, microphone, sinOsc_)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (getMicrophoneAndCamera)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<section>
  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so.</p>

  <blockquote>Make sure to use 🎧 when you run this example! Otherwise, you'll cause quite a stir in whatever internet cafe, household or public restroom you're perusing this documentation in.</blockquote>

  <pre><code>\mic -> run2_
  [ case mic of
      Just m -> fix \i -> gain_ 1.0
        [ microphone m
        , delay_ 0.1 [ gain_ 0.2 [ input i ] ]
        ]
      Nothing -> gain_ 0.02 [ sinOsc_ 440.0 ]
  ]</code></pre>

  @microphone@
  </section>
"""

microphoneEx
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
microphoneEx ccb _ ev = makePursx' (Proxy :: _ "@") px
  { microphone: nut
      (toDOM $ audioWrapper ev ccb (\_ -> getMicrophoneAndCamera true false)
          \ctx { microphone: mic } -> run2 ctx
            [ case mic of
                Just m -> fix \i -> gain_ 1.0
                  [ microphone m
                  , delay_ 0.1 [ gain_ 0.2 [ i ] ]
                  ]
                Nothing -> gain_ 0.02 [ sinOsc_ 440.0 ]
            ]
      )
  }