module WAGS.Example.Docs.AudioUnits.Microphone where

import Prelude

import Control.Plus (class Plus)
import Data.Maybe (Maybe(..))
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (delay_, gain_, microphone, sinOsc_, (~), (:*))
import WAGS.Core (fix, input)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (getMicrophoneAndCamera)
import WAGS.Run (run2_)

px =
  Proxy    :: Proxy         """<section>
  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so. The <code>fix</code> in the example below will be explained more in the "Fan and fix" section.</p>

  <blockquote>Make sure to use 🎧 when you run this example! Otherwise, you'll cause quite a stir in whatever internet cafe, household or public restroom you're perusing this documentation in.</blockquote>

  <pre><code>\mic -> run2_
  case mic of
    Just m -> fix \i -> gain_ 1.0
      $ microphone m ~
          (delay_ 0.1 $ gain_ 0.2 $ input i)
    Nothing -> gain_ 0.02 $ sinOsc_ 440.0
</code></pre>

  @microphone@
  </section>
"""

microphoneEx
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
microphoneEx ccb _ ev = makePursx' (Proxy :: _ "@") px
  { microphone: nut
      ( audioWrapper ev ccb (getMicrophoneAndCamera true false)
          \{ microphone: mic } -> run2_
            case mic of
              Just m -> fix \i -> gain_ 1.0
                $ microphone m ~
                    (delay_ 0.1 $ gain_ 0.2 $ input i)
              Nothing -> gain_ 0.02 $ sinOsc_ 440.0
      )
  }