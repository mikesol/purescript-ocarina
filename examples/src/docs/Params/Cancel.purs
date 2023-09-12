module Ocarina.Example.Docs.Params.Cancel where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Array ((..))
import Data.Either (Either(..), isLeft, isRight)
import Data.Filterable (filter)
import Data.Foldable (oneOf)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.Pursx (pursx)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, forkAff)
import Effect.Class (liftEffect)
import FRP.Poll (Poll, create)
import Ocarina.Control (gain_, loopBuf)
import Ocarina.Core (AudioCancel(..), AudioEnvelope(..), bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Properties (playbackRate)
import Ocarina.Run (run2)

cancelEx :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
cancelEx ccb _ ev = pursx @Px
  { txt:
      ( text_
          """\ctx { buf, poll } -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , filter isLeft poll $>
                  ( playbackRate
                      $ AudioEnvelope
                          { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                          , o: 1.5
                          , d: 30.0
                          }
                  )
              , filter isRight poll $> (playbackRate (AudioCancel { o: 3.5 }))
              ]
          )
      ]
  ]
"""
      )
  , cancel:
      ( audioWrapper ev ccb
          ( \ctx -> do
              buf <- decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3"
              { poll, push } <- liftEffect $ liftST $ create
              _ <- forkAff do
                delay (Milliseconds 1000.0)
                liftEffect $ push $ Left unit
                delay (Milliseconds 3000.0)
                liftEffect $ push $ Right unit
              pure { buf, poll }
          )
          \ctx { buf, poll } -> run2 ctx
            [ gain_ 1.0
                [ loopBuf buf
                    ( oneOf
                        [ bangOn
                        , filter isLeft poll $>
                            ( playbackRate
                                $ AudioEnvelope
                                    { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                                    , o: 1.5
                                    , d: 30.0
                                    }
                            )
                        , filter isRight poll $> (playbackRate (AudioCancel { o: 3.5 }))
                        ]
                    )
                ]
            ]
      )
  }

type Px =
  """<section>
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