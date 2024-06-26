module Ocarina.Example.Docs.Params.Numeric where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Either (Either(..), isLeft, isRight)
import Data.Filterable (filter)
import Deku.Core (Nut)
import Deku.Pursx (pursx')
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, forkAff)
import Effect.Class (liftEffect)
import FRP.Poll (Poll, create)
import Ocarina.Control (gain_, loopBuf)
import Ocarina.Core (AudioNumeric(..), _exponential, _linear, _step, bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Properties (playbackRate)
import Ocarina.Run (run2)
import QualifiedDo.Alt as OneOf

numericEx
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
numericEx ccb _ ev = pursx' @"@" @Px
  { numericEx:
      ( audioWrapper ev ccb
          ( \ctx -> do
              buf <- decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3"
              { poll, push } <- liftEffect $ liftST $ create
              _ <- forkAff do
                delay (Milliseconds 1000.0)
                liftEffect $ push (Left unit)
                delay (Milliseconds 2500.0)
                liftEffect $ push (Right unit)
              pure { buf, poll }
          )
          \ctx { buf, poll } -> run2 ctx
            [ gain_ 1.0
                [ loopBuf buf OneOf.do
                    bangOn
                    filter isLeft poll $>
                      ( playbackRate
                          $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
                      )
                    filter isLeft poll $>
                      ( playbackRate
                          $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
                      )
                    filter isRight poll $>
                      ( playbackRate
                          $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
                      )
                    filter isRight poll $>
                      ( playbackRate
                          $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
                      )
                ]
            ]
      )
  }

type Px =
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

  <pre><code>\ctx { buf, poll } -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          filter isLeft poll $>
            ( playbackRate
                $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            )
          filter isLeft poll $>
            ( playbackRate
                $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
            )
          filter isRight poll $>
            ( playbackRate
                $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            )
          filter isRight poll $>
            ( playbackRate
                $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
            )
      ]
  ]</code></pre>

  @numericEx@
  </section>
"""