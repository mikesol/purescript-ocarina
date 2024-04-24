module Ocarina.Example.Docs.Params.Sudden where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Foldable (oneOf)
import Deku.Core (Nut)
import Deku.Pursx (pursx')
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, forkAff)
import Effect.Class (liftEffect)
import FRP.Poll (Poll, create)
import Ocarina.Control (gain_, loopBuf)
import Ocarina.Core (AudioSudden(..), bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Properties (playbackRate)
import Ocarina.Run (run2)

suddenEx
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
suddenEx ccb _ ev = pursx' @"@" @Px
  { suddenEx:
      ( audioWrapper ev ccb
          ( \ctx -> do
              buf <- decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3"
              { poll, push } <- liftEffect $ liftST $ create
              _ <- forkAff do
                delay (Milliseconds 1500.0)
                liftEffect $ push unit
              pure { buf, poll }
          )
          \ctx { buf, poll } -> run2 ctx
            [ gain_ 1.0
                [ loopBuf buf
                    ( oneOf
                        [ bangOn
                        , poll $> (playbackRate $ AudioSudden { n: 1.4 })
                        ]
                    )
                ]
            ]
      )
  }

type Px =
  """<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\ctx { buf, poll } -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , poll $> (playbackRate $ AudioSudden { n: 1.4 })
              ]
          )
      ]
  ]</code></pre>

  @suddenEx@
  </section>
"""