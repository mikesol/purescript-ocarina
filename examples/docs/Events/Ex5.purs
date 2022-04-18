module WAGS.Example.Docs.Events.Ex5 where

import Prelude

import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential)
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
import WAGS.Control (gain_, microphone, playBuf, recorder, singleton, speaker2, (~))
import WAGS.Core (AudioInput)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (WrapperStates(..), audioWrapper, clickCb, mkWrapperEvent)
import WAGS.Interpret (close, context, contextState, decodeAudioDataFromUri, effectfulAudioInterpret, getMicrophoneAndCamera, makeFFIAudioSnapshot, mediaRecorderToUrl)
import WAGS.Parameter (apOn, dt)
import WAGS.Properties (onOff)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserMicrophone, MediaRecorderCb(..))

px =
  Proxy    :: Proxy   """<section>
  <h2>Example 6: An audio player with behaviors</h2>

  <p>Behaviors are a great way to create interactive works where user actions are decoupled from rendering. In the following example, the rendering is consistently once a second, but the audio buffer be changed as often or as little as we want. The ticker event samples the buffer behavior to grab the current buffer.</p>

  <pre><code>placeholder</code></pre>

  @ex5@

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

ex5
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
ex5 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ex5: nut
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
