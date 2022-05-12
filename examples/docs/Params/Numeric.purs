module WAGS.Example.Docs.Params.Numeric where

import Prelude

import Deku.Core (Domable, envy)
import QualifiedDo.Alt as OneOf
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event, bang)
import FRP.Event (delay)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, loopBuf)
import WAGS.Core (AudioNumeric(..), _exponential, _linear, _step, bangOn)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Properties (playbackRate)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy
         """<section>
  <h2>AudioNumeric</h2>
  <p><code>AudioNumeric</code> encompasses the following three functions from the Web Audio API:</p>

  <ul>
    <li><code>linearRampToValueAtTime</code> via the <code>_linear</code> transition.</li>
    <li><code>exponentialRampToValueAtTime</code> via the <code>_exponential</code> transition.</li>
    <li><code>setValueAtTime</code> via the <code>_step</code> transition.</li>
  </ul>

  <p>Let's explore all of them in the example below.</p>

  <blockquote>Pro tip: When using <code>AudioNumeric</code>, consider starting with a <code>_step</code> transition. Otherwise, the transition may be abrupt and unpleasant!</blockquote>

  <pre><code>\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000 OneOf.do
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
          delay 2500 OneOf.do
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            bang
              $ playbackRate
              $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
      ]
  ]</code></pre>

  @numericEx@
  </section>
"""

numericEx
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
numericEx ccb _ ev = makePursx' (Proxy :: _ "@") px
  { numericEx: nut
      ( envy $ audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ gain_ 1.0
                [ loopBuf buf OneOf.do
                    bangOn
                    delay 1000 OneOf.do
                      bang
                        $ playbackRate
                        $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
                      bang
                        $ playbackRate
                        $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }

                    delay 2500 OneOf.do
                      bang
                        $ playbackRate
                        $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
                      bang
                        $ playbackRate
                        $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
                ]
            ]
      )
  }