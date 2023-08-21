module Ocarina.Example.Docs.AudioUnits.Recorder where

import Prelude

import Bolson.Core (Entity)
import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as RRef
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState')
import Deku.Pursx ((~~))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (launchAff, launchAff_, try)
import Effect.Class (liftEffect)
import FRP.Event (create, subscribe)
import FRP.Poll (Poll, sample)
import Ocarina.Common (class InitialMicrophone, class InitialRecorder)
import Ocarina.Control (microphone, recorder, speaker2)
import Ocarina.Core (Node)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import Ocarina.Example.Docs.Util (WrapperStates(..), mkWrapperEvent, raceSelf)
import Ocarina.Interpret (close, context, contextState, effectfulAudioInterpret, getMicrophoneAndCamera, makeFFIAudioSnapshot, mediaRecorderToUrl, stopMediaRecorder)
import Ocarina.WebAPI (MediaRecorder, MediaRecorderCb(..))
import Type.Proxy (Proxy(..))

type RecorderStates = Either (Either String MediaRecorder) WrapperStates

scene :: forall i7 outputChannels8 payload9 i11. InitialRecorder i7 => InitialMicrophone i11 => i11 -> i7 -> Entity Void (Node outputChannels8 payload9)
scene m cb = recorder cb (microphone m)

recorderEx
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
recorderEx ccb _ ev = px ~~
  { recorder: Deku.do
      push /\ event' <- useState' -- bussed \push (event' :: Event RecorderStates) ->
      let
        ptn = partitionMap identity event'
        event = mkWrapperEvent ev (_.right ptn)
        ls = partitionMap identity (_.left ptn)
        aEv = _.left ls
        rEv = _.right ls

      D.div_
        [ D.button
            [ DA.style_ "cursor: pointer;"
            , DL.runOn DL.click $
                ( map
                    ( \{ e, cncl, rec } -> case e of
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
                                    ep <- liftST create
                                    ffi2 <- makeFFIAudioSnapshot ctx
                                    rf <- liftST (RRef.new 0)
                                    rf2 <- liftST $ RRef.new Map.empty
                                    let exec audio = audio ffi2
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
                                        (effectfulAudioInterpret rf rf2 exec) 
                                    unsub <- liftST $ subscribe (sample audioE ep.event) exec
                                    ep.push identity
                                    pure do
                                      liftST unsub
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
                  ( (map ($) ((map { e: _, cncl: _, rec: _ } event) <*> (pure (pure unit) <|> (map (\(SetCancel x) -> x) ev)))) <*>
                      (pure Nothing <|> map Just rEv)

                  )
            ]
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
                [ DA.controls_ "true"
                , DA.style_  "display:none;"
                , DA.src aEv
                , DA.style $ aEv $>  "display:block;"
                ]
                []
            ]
        ]
  }

px =
  Proxy
    :: Proxy
         """<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app 🎙️.</p>

  <pre><code>\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
"""