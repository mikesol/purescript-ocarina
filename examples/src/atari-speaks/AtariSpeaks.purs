module Ocarina.Example.AtariSpeaks where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as RRef
import Data.Array ((..))
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable (for_, intercalate, fold)
import Data.Lens (over, view)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (pi, sin)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Data.UInt (toInt)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as DOM
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create, filterMap, subscribe)
import FRP.Event.AnimationFrame (animationFrame')
import FRP.Poll (Poll, sample, sham)
import Ocarina.Clock (WriteHead, fot, withWriteHead)
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
  -> WriteHead Poll
  -> Audible D2 payload
scene buffer cb wh =
  let
    tr = fot wh (mul pi)
  in
    analyser_ { cb }
      [ gain_ 1.0
          [ gain_ 0.3
              [ loopBuf { buffer, playbackRate: 1.0 }
                  ( bangOn <|>
                      playbackRate <<<
                        over opticN (\rad -> 1.0 + 0.1 * sin rad) <$> tr
                  )
              ]
          , gain_ 0.15
              [ loopBuf { buffer, playbackRate: 1.0 }
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
          , gain_ 0.3 [ loopBuf { buffer, playbackRate: 0.25 } bangOn ]
          ]
      ]

data UIAction
  = TurnOff { ctx :: AudioContext, unsub :: Effect Unit }
  | AsciiMixer String
  | TurnOn

type Init = BrowserAudioBuffer

initializeAtariSpeaks :: Aff Init
initializeAtariSpeaks =
  liftEffect context
    >>= flip decodeAudioDataFromUri
      "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"

atariSpeaks
  :: BrowserAudioBuffer
  -> RaiseCancellation
  -> Nut
atariSpeaks atari rc = Deku.do
  push /\ event <- useState TurnOn
  DOM.div_
    [ DOM.h1_ [ text_ "Atari speaks" ]
    , DOM.button
        [ DL.runOn DL.click $ map
            ( case _ of
                Nothing -> do
                  analyserE <- liftST create
                  start <- liftST create
                  ctx <- context
                  rf0 <- liftST $ RRef.new 0
                  rf1 <- liftST $ RRef.new Map.empty
                  ffi2 <- makeFFIAudioSnapshot ctx
                  afe <- animationFrame' (withWriteHead 0.04 ctx)
                  let exec audio = audio ffi2
                  let
                    audioE = speaker2
                      [ scene atari
                          ( AnalyserNodeCb
                              ( \a -> do
                                  analyserE.push (Just a)
                                  pure (analyserE.push Nothing)
                              )
                          )
                          (sham $ map _.ac (afe.event))
                      ]
                      (effectfulAudioInterpret rf0 rf1 exec)

                  unsub0 <- subscribe (sample audioE start.event) exec
                  unsub1 <- subscribe analyserE.event \analyser -> do
                    for_ analyser \a -> do
                      frequencyData <- getByteFrequencyData a
                      arr <- toArray frequencyData
                      push $ AsciiMixer $ intercalate "\n" $
                        map (\ii -> fold ((0 .. toInt ii) $> ">")) arr
                  start.push identity
                  let unsub = unsub0 *> unsub1 *> afe.unsubscribe
                  rc $ Just { unsub, ctx }
                  push $ TurnOff { unsub, ctx }
                Just { unsub, ctx } -> do
                  unsub
                  close ctx
                  rc Nothing
                  push TurnOn

            )
            ( event # filterMap case _ of
                AsciiMixer _ -> Nothing
                TurnOff a -> Just (Just a)
                TurnOn -> Just Nothing
            )
        ]
        [ text
            ( event <#> case _ of
                TurnOn -> "Turn on"
                _ -> "Turn off"
            )
        ]
    , DOM.p_
        [ text
            ( event <#> case _ of
                AsciiMixer s -> s
                _ -> ""
            )
        ]
    ]

main :: Effect Unit
main = launchAff_ do
  atari <- initializeAtariSpeaks
  liftEffect $ runInBody (atariSpeaks atari (const $ pure unit))
