module Ocarina.Example.Docs.Params.Envelope where

import Prelude

import Data.Array ((..))
import Data.Foldable (oneOf)
import Deku.Control (text_)
import Deku.Core (Domable, envy)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event, bang)
import FRP.Event (delay)
import Type.Proxy (Proxy(..))
import Ocarina.Control (gain_, loopBuf)
import Ocarina.Core (AudioEnvelope(..), bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Properties (playbackRate)
import Ocarina.Run (run2)

px =
  Proxy    :: Proxy         """<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
"""

envelopeEx :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
envelopeEx ccb _ ev = px ~~
  { txt: nut
      ( text_
          """\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          OneOf.do
            bangOn
            delay 1000
              $ bang
              $ playbackRate
              $ AudioEnvelope
                  { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                  , o: 1.5
                  , d: 30.0
                  }
          )
      ]
  ]"""
      )
  , envelope: nut
      ( envy $ audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ gain_ 1.0
                [ loopBuf buf
                    ( oneOf
                        [ bangOn
                        , delay 1000
                            $ bang
                            $ playbackRate
                            $ AudioEnvelope
                                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                                , o: 1.5
                                , d: 30.0
                                }
                        ]
                    )
                ]
            ]
      )
  }