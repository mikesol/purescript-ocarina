module Ocarina.Example.Docs.Params.Cancel where

import Prelude

import Data.Array ((..))
import Data.Foldable (oneOf)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Event (delay_)
import FRP.Poll (Poll, dredge)
import Ocarina.Control (gain_, loopBuf)
import Ocarina.Core (AudioCancel(..), AudioEnvelope(..), bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Properties (playbackRate)
import Ocarina.Run (run2)
import Type.Proxy (Proxy(..))

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

cancelEx :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
cancelEx ccb _ ev = px ~~
  { txt:
      ( text_
          """\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000
            $ pure
            $ playbackRate
            $ AudioEnvelope
                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                , o: 1.5
                , d: 30.0
                }
          delay 3000 (pure (playbackRate (AudioCancel { o: 3.5 })))
      ]
  ]"""
      )
  , cancel:
      (audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ gain_ 1.0
                [ loopBuf buf
                    ( oneOf
                        [ bangOn
                        , dredge (delay_ 1000)
                            $ pure
                            $ playbackRate
                            $ AudioEnvelope
                                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                                , o: 1.5
                                , d: 30.0
                                }
                        , dredge (delay_ 3000) (pure (playbackRate (AudioCancel { o: 3.5 })))
                        ]
                    )
                ]
            ]
      )
  }