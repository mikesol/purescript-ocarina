module Ocarina.Example.AtariSpeaks where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as RRef
import Data.Array ((..))
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable (for_, intercalate, fold)
import Data.Lens (over, view)
import Data.Maybe (Maybe(..))
import Data.Number (pi, sin)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Data.UInt (toInt)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Nut, bussed)
import Deku.DOM as DOM
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (Event, create, filterMap, hot, sampleOnRight, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import Ocarina.Clock (WriteHead, fot, writeHead)
import Ocarina.Control (analyser_, gain_, loopBuf, speaker2)
import Ocarina.Core (Audible, bangOn, opticN)
import Ocarina.Example.Utils (RaiseCancellation)
import Ocarina.Interpret (close, context, decodeAudioDataFromUri, effectfulAudioInterpret, getByteFrequencyData, makeFFIAudioSnapshot)
import Ocarina.Properties (loopEnd, loopStart, playbackRate)
import Ocarina.WebAPI (AnalyserNodeCb(..), AudioContext, BrowserAudioBuffer)

scene
  :: forall payload
   . BrowserAudioBuffer
  -> AnalyserNodeCb
  -> WriteHead Event
  -> Audible D2 payload
scene atar cb wh =
  let
    tr = fot wh (mul pi)
  in
    analyser_ { cb }
      [ gain_ 1.0
          [ gain_ 0.3
              [ loopBuf { buffer: atar, playbackRate: 1.0 }
                  ( bangOn <|>
                      playbackRate <<<
                        over opticN (\rad -> 1.0 + 0.1 * sin rad) <$> tr
                  )
              ]
          , gain_ 0.15
              [ loopBuf { buffer: atar, playbackRate: 1.0 }
                  ( bangOn
                      <|>
                        playbackRate <<<
                          over opticN (\rad -> 1.5 + 0.1 * sin (2.0 * rad))
                          <$> tr
                      <|>
                        loopStart <<< (\rad -> 0.1 + 0.1 * sin rad)
                          <<< view opticN <$> tr
                      <|>
                        loopEnd
                          <<< (\rad -> 0.5 + 0.25 * sin (2.0 * rad))
                          <<< view opticN <$> tr
                  )
              ]
          , gain_ 0.3
              [ loopBuf { buffer: atar, playbackRate: 0.25 } bangOn ]
          ]
      ]

data UIAction
  = TurnOff { ctx :: AudioContext, unsub :: Effect Unit }
  | AsciiMixer String
  | TurnOn

type Init = BrowserAudioBuffer

initializeAtariSpeaks :: Aff Init
initializeAtariSpeaks = do
  atar <- liftEffect context >>= flip decodeAudioDataFromUri
    "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"
  pure atar

atariSpeaks
  :: BrowserAudioBuffer
  -> RaiseCancellation
  -> Nut
atariSpeaks atar rc = bussed \push -> lcmap (alt (pure TurnOn)) \event ->
  DOM.div_
    [ DOM.h1_ [ text_ "Atari speaks" ]
    , DOM.button
        [ map
            ( \i -> DOM.OnClick := cb
                ( const $
                    case i of
                      Nothing -> do
                        analyserE <- create
                        ctx <- context
                        rf <- liftST $ RRef.new 0
                        ffi2 <- makeFFIAudioSnapshot ctx
                        afe <- hot animationFrameEvent
                        let wh = writeHead 0.04 ctx
                        let
                          audioE = speaker2
                            [ scene atar
                                ( AnalyserNodeCb
                                    ( \a -> do
                                        analyserE.push (Just a)
                                        pure (analyserE.push Nothing)
                                    )
                                )
                                (sample_ wh afe.event)
                            ]
                            (effectfulAudioInterpret rf)

                        unsub' <- subscribe
                          ( sampleOnRight
                              (analyserE.event <|> pure Nothing)
                              (map Tuple audioE)
                          )
                          ( \(Tuple audio analyser) -> do
                              audio ffi2
                              for_ analyser \a -> do
                                frequencyData <-
                                  getByteFrequencyData a
                                arr <- toArray frequencyData
                                push $ AsciiMixer $
                                  intercalate "\n"
                                    ( map
                                        ( \ii ->
                                            fold
                                              ( ( 0 ..
                                                    toInt ii
                                                ) $> ">"
                                              )

                                        )
                                        arr
                                    )
                          )
                        let unsub = unsub' *> afe.unsubscribe
                        rc $ Just { unsub, ctx }
                        push $ TurnOff { unsub, ctx }
                      Just { unsub, ctx } -> do
                        unsub
                        close ctx
                        rc Nothing
                        push TurnOn

                )
            )
            ( event # filterMap case _ of
                AsciiMixer _ -> Nothing
                TurnOff a -> Just (Just a)
                TurnOn -> Just Nothing
            )
        ]
        [ text
            ( event # map case _ of
                TurnOn -> "Turn on"
                _ -> "Turn off"
            )
        ]
    , DOM.p_
        [ text
            ( event # map case _ of
                AsciiMixer s -> s
                _ -> ""
            )
        ]
    ]

main :: Effect Unit
main = launchAff_ do
  atar <- initializeAtariSpeaks
  liftEffect $ runInBody (atariSpeaks atar (const $ pure unit))
