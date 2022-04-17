module WAGS.Example.Docs.Events.Ex4 where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Filterable (partitionMap)
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute ((:=))
import Deku.Control (text)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Effect (Effect)
import FRP.Event (class IsEvent, Event, subscribe)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Control (microphone, recorder, singleton, speaker2)
import WAGS.Core (AudioInput)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (WrapperStates(..), clickCb, mkWrapperEvent)
import WAGS.Interpret (close, context, contextState, effectfulAudioInterpret, getMicrophoneAndCamera, makeFFIAudioSnapshot, mediaRecorderToUrl)
import WAGS.WebAPI (BrowserMicrophone, MediaRecorderCb(..))

px =
  Proxy    :: Proxy   """<section>
    <h2>Example 5: Following the mouse</h2>

  <p>This example shows how to respond to the mouse's position. Rather than using an event listener, which can fire way too fast than is necessary for rendering, we'll use a behavior with the mouse's position and sample it via <code>requestAnimationFrame</code>.</p>

  <pre><code>placeholder</code></pre>

  @ex4@

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

</section>
"""


type RecorderStates = Either String WrapperStates

scene
  :: forall payload
   . BrowserMicrophone
  -> MediaRecorderCb
  -> AudioInput D2 "" () Event payload
scene m cb =
  singleton
    (recorder cb (microphone m))

ex4
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
ex4 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ex4: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push (event' :: event RecorderStates) ->
            let
              ptn = partitionMap identity event'
              event = mkWrapperEvent ev (_.right ptn)
              aEv = _.left ptn
            in
              D.div_
                [ D.button
                    ( (bang (D.Style := "cursor: pointer;")) <|>
                        ( clickCb ccb (Right >>> push)
                            (_.microphone <$> getMicrophoneAndCamera true false)
                            ( maybe (pure $ pure unit) \mc -> do
                                ctx <- context
                                ffi2 <- makeFFIAudioSnapshot ctx
                                let
                                  audioE = speaker2
                                    ( scene mc
                                        ( MediaRecorderCb $ mediaRecorderToUrl
                                            "audio/ogg; codecs=opus"
                                            (Left >>> push)
                                        )
                                    )
                                    effectfulAudioInterpret
                                unsub <- subscribe
                                  audioE
                                  ( \audio -> audio ffi2
                                  )
                                pure do
                                  unsub
                                  contextState ctx >>= \st -> when (st /= "closed") (close ctx)
                            )
                            ev
                            event
                        )
                    )
                    [ text
                        ( map
                            ( case _ of
                                Stopped -> "Turn on"
                                Loading -> "Loading..."
                                Playing _ -> "Turn off"
                            )
                            event
                        )
                    ]
                , D.div_ [ D.audio ((bang (D.Controls := "true")) <|> (bang (D.Style := "display:none;")) <|> map (\src -> (D.Src := src)) aEv <|> map (const (D.Style := "display:block;")) aEv )  [] ]
                ]
      )
  }
