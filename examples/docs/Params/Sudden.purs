module WAGS.Example.Docs.Params.Sudden where

import Prelude

import Data.Foldable (oneOf)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event, bang)
import FRP.Event.Time (delay)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, loopBuf)
import WAGS.Core (AudioSudden(..), bangOn)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Properties (playbackRate)
import WAGS.Run (run2)

px =
  Proxy    :: Proxy         """<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value">`value`</a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1500
                  $ bang
                  $ playbackRate
                  $ AudioSudden { n: 1.4 }
              ]
          )
      ]
  ]</code></pre>

  @suddenEx@
  </section>
"""

suddenEx
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
suddenEx ccb _ ev = makePursx' (Proxy :: _ "@") px
  { suddenEx: nut
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \ctx buf -> run2 ctx
            [ gain_ 1.0
                [ loopBuf buf
                    ( oneOf
                        [ bangOn
                        , delay 1500
                            $ bang
                            $ playbackRate
                            $ AudioSudden { n: 1.4 }
                        ]
                    )
                ]
            ]
      )
  }