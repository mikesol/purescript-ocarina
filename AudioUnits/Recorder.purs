module WAGS.Example.Docs.AudioUnits.Recorder where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Filterable (partitionMap)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (blank, plant, text)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (launchAff, launchAff_, try)
import Effect.Class (liftEffect)
import FRP.Event (Event, bus, subscribe)
import FRP.Event.Class (bang, biSampleOn)
import Type.Proxy (Proxy(..))
import WAGS.Control (microphone, recorder, speaker2)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Example.Docs.Util (WrapperStates(..), mkWrapperEvent, raceSelf)
import WAGS.Interpret (close, context, contextState, effectfulAudioInterpret, getMicrophoneAndCamera, makeFFIAudioSnapshot, mediaRecorderToUrl, stopMediaRecorder)
import WAGS.WebAPI (MediaRecorder, MediaRecorderCb(..))

px =
  Proxy    :: Proxy         """<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app 🎙️.</p>

  <pre><code>\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
"""

type RecorderStates = Either (Either String MediaRecorder) WrapperStates

scene m cb = recorder cb (microphone m)

recorderEx
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
recorderEx ccb _ ev = px ~~
  { recorder: nut
      ( bus \push (event' :: Event RecorderStates) ->
            let
              ptn = partitionMap identity event'
              event = mkWrapperEvent ev (_.right ptn)
              ls = partitionMap identity (_.left ptn)
              aEv = _.left ls
              rEv = _.right ls
            in
              plant $ D.div_
                [ D.button
                    ( (bang (D.Style := "cursor: pointer;")) <|>
                        ( map
                            ( \{ e, cncl, rec } -> D.OnClick :=
                                ( cb $
                                    ( const $ case e of
                                        Loading -> pure unit
                                        Playing x -> x *> ccb (pure unit)
                                          *> for_ rec (try <<< stopMediaRecorder)
                                          *> (Right >>> push) Stopped
                                        Stopped -> do
                                          cncl
                                          av <- AVar.empty
                                          push (Right Loading)
                                          fib <- launchAff do
                                            x <- (_.microphone <$> getMicrophoneAndCamera true false)
                                            liftEffect do
                                              res <-
                                                ( maybe (pure $ pure unit) \mc -> do
                                                    ctx <- context
                                                    ffi2 <- makeFFIAudioSnapshot ctx
                                                    let
                                                      audioE = speaker2
                                                        [ scene mc
                                                            ( MediaRecorderCb $ \mr -> do
                                                                push (Left (Right mr))
                                                                void $ AVar.tryPut mr av
                                                                mediaRecorderToUrl
                                                                  "audio/ogg; codecs=opus"
                                                                  (Left >>> Left >>> push)
                                                                  mr
                                                            )
                                                        ]
                                                        effectfulAudioInterpret
                                                    unsub <- subscribe
                                                      audioE
                                                      ( \audio -> audio ffi2
                                                      )
                                                    pure do
                                                      unsub
                                                      AVar.tryTake av >>= traverse_ (try <<< stopMediaRecorder)
                                                      contextState ctx >>= \st -> when (st /= "closed") (close ctx)
                                                ) x
                                              push (Right $ Playing res)
                                              pure res
                                          ccb do
                                            (Right >>> push) Stopped
                                            launchAff_ $ raceSelf fib
                                          pure unit
                                    )
                                )
                            )
                            ( biSampleOn (bang Nothing <|> map Just rEv)
                                (map ($) (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map { e: _, cncl: _, rec: _ } event)))
                            )
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
                , D.div_
                    [ D.audio
                        ( (bang (D.Controls := "true")) <|> (bang (D.Style := "display:none;")) <|> map (\src -> (D.Src := src)) aEv <|> map
                            (const (D.Style := "display:block;"))
                            aEv
                        )
                        blank
                    ]
                ]
      )
  }