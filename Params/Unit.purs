module WAGS.Example.Docs.Params.Unit where

import Prelude

import Data.Foldable (oneOf)
import Deku.Core (Domable, envy)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event, bang)
import Type.Proxy (Proxy(..))
import WAGS.Control (constant, gain_, loopBuf, lowpass_, squareOsc)
import WAGS.Core (bangOn, c1)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Properties (playbackRate)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\ctx buf -> run2 ctx
  [ loopBuf buf OneOf.do
      bangOn
      bang
        $ playbackRate
        $ c1
            ( gain_ 1.0
                [ constant 1.0 bangOn
                , gain_ 0.2 (lowpass_ 100.0 (squareOsc 50.0 bangOn))
                ]
            )
      )
  ]
</code></pre>

  ~unitEx~
  </section>
"""

unitEx :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
unitEx ccb _ ev = px ~~
  { unitEx: nut
      ( envy $ audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ loopBuf buf
                ( oneOf
                    [ bangOn
                    , bang
                        $ playbackRate
                        $ c1
                            ( gain_ 1.0
                                [ constant 1.0 bangOn
                                , gain_ 0.2 [lowpass_ 100.0 [squareOsc 50.0 bangOn]]
                                ]
                            )
                    ]
                )
            ]
      )
  }