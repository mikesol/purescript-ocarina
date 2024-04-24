module Ocarina.Example.Docs.Params.Envelope where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Foldable (oneOf)
import Deku.Core (Nut)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, forkAff)
import Effect.Class (liftEffect)
import FRP.Poll (Poll, create)
import Data.Array ((..))
import Deku.Control (text_)
import Deku.Pursx (pursx)
import Ocarina.Control (gain_, loopBuf)
import Ocarina.Core (AudioEnvelope(..), bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Properties (playbackRate)
import Ocarina.Run (run2)

envelopeEx :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
envelopeEx ccb _ ev = pursx @Px
  { txt:
      ( text_
          """\ctx { buf, poll } -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , poll $>
                  ( playbackRate
                      $ AudioEnvelope
                          { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                          , o: 1.5
                          , d: 30.0
                          }
                  )
              ]
          )
      ]
  ]"""
      )
  , envelope:
      ( audioWrapper ev ccb
          ( \ctx -> do
              buf <- decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3"
              { poll, push } <- liftEffect $ liftST $ create
              _ <- forkAff do
                delay (Milliseconds 1000.0)
                liftEffect $ push unit
              pure { buf, poll }
          )
          \ctx { buf, poll } -> run2 ctx
            [ gain_ 1.0
                [ loopBuf buf
                    ( oneOf
                        [ bangOn
                        , poll $>
                            ( playbackRate
                                $ AudioEnvelope
                                    { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                                    , o: 1.5
                                    , d: 30.0
                                    }
                            )
                        ]
                    )
                ]
            ]
      )
  }

type Px =
  """<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
"""