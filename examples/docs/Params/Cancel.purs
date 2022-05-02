module WAGS.Example.Docs.Params.Cancel where

import Prelude

import Data.Array ((..))
import Data.Foldable (oneOf)
import Deku.Control (text_)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event, bang)
import FRP.Event.Time (delay)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, loopBuf)
import WAGS.Core (AudioCancel(..), AudioEnvelope(..), bangOn)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Properties (playbackRate)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<section>
  <h2>Cancel</h2>
  <p>The <code>AudioCancel</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelScheduledValues"><code>cancelScheduledValues</code></a> function and cancels whatever effects you programmed in the future. In the example below, we execute the following sequence:</p>
  <ol>
    <li>Play an audio file</li>
    <li>Send an event at 1.0 seconds to schedule an evenlope to modulate the audio rate starting at 1.5 seconds.</li>
    <li>Cancel the envelope at 3.0 seconds, but schedule the cancelation to take effect at 4.0 seconds.</li>
  </ol>
  <pre><code>~txt~</code></pre>
  ~cancel~
  </section>
"""

cancelEx :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
cancelEx ccb _ ev = px ~~
  { txt: nut
      ( text_
          """\ctx buf -> run2 ctx
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
              , delay 3000 (bang (playbackRate (AudioCancel { o: 3.5 })))
              ]
          )
      ]
  ]"""
      )
  , cancel: nut
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/339/339822_5121236-lq.mp3")
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
                        , delay 3000 (bang (playbackRate (AudioCancel { o: 3.5 })))
                        ]
                    )
                ]
            ]
      )
  }