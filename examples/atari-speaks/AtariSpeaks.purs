module WAGS.Example.AtariSpeaks where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.ArrayBuffer.Typed (toArray)
import Data.Either (either)
import Data.Exists (Exists, mkExists)
import Data.Foldable (for_, intercalate)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.String.Utils (unsafeRepeat)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Data.UInt (toInt)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, text, text_)
import Deku.Core (SubgraphF(..))
import Deku.DOM as DOM
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, create, filterMap, sampleOn, subscribe)
import FRP.Event.Phantom (PhantomEvent, toEvent)
import Math (pi, sin)
import WAGS.Control (analyser, gain, gain__, loopBuf, singleton, speaker2, (:*))
import WAGS.Core (GainInput)
import WAGS.Example.Utils (RaiseCancellation, animationFrameEvent)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, effectfulAudioInterpret, getByteFrequencyData, makeFFIAudioSnapshot, writeHead)
import WAGS.Parameter (WriteHead, at_, ovnn, pureOn, vwnn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.WebAPI (AnalyserNodeCb(..), AudioContext, BrowserAudioBuffer)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

scene
  :: forall event payload
   . IsEvent event
  => BrowserAudioBuffer
  -> AnalyserNodeCb
  -> WriteHead event
  -> GainInput D2 () () event payload
scene atar cb wh =
  let
    tr = at_ wh (mul pi)
  in
    singleton
      ( analyser { cb } empty
          ( gain 1.0 empty
              $
                gain__ 0.3 empty
                  ( loopBuf { buffer: atar, playbackRate: 1.0 }
                      ( pureOn <|>
                          playbackRate <<<
                            (ovnn (\rad -> 1.0 + 0.1 * sin rad)) <$> tr
                      )
                  )
                  :*
                    [ gain__ 0.15 empty
                        ( loopBuf { buffer: atar, playbackRate: 1.0 }
                            ( pureOn
                                <|>
                                  playbackRate <<<
                                    (ovnn (\rad -> 1.5 + 0.1 * sin (2.0 * rad)))
                                    <$> tr
                                <|>
                                  loopStart <<< (\rad -> 0.1 + 0.1 * sin rad)
                                    <<< vwnn <$> tr
                                <|>
                                  loopEnd
                                    <<< (\rad -> 0.5 + 0.25 * sin (2.0 * rad))
                                    <<< vwnn <$> tr
                            )
                        )
                    , gain__ 0.3 empty
                        (loopBuf { buffer: atar, playbackRate: 0.25 } pureOn)
                    ]
          )
      )

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
  :: forall payload
   . BrowserAudioBuffer
  -> RaiseCancellation
  -> Exists (SubgraphF Unit PhantomEvent payload)
atariSpeaks atar rc = mkExists $ SubgraphF \push -> lcmap
  (map (either (const $ TurnOn) identity))
  \event ->
    DOM.div_
      [ DOM.h1_ [ text_ "Atari speaks" ]
      , DOM.button
          ( map
              ( \i -> DOM.OnClick := cb
                  ( const $
                      case i of
                        Nothing -> do
                          analyserE <- create
                          ctx <- context
                          ffi2 <- makeFFIAudioSnapshot ctx
                          let wh = writeHead 0.04 ctx
                          let
                            audioE = speaker2
                              ( scene atar
                                  ( AnalyserNodeCb
                                      ( \a -> do
                                          analyserE.push (Just a)
                                          pure (analyserE.push Nothing)
                                      )
                                  )
                                  (sample_ wh animationFrameEvent)
                              )
                              effectfulAudioInterpret

                          unsub <- subscribe
                            ( sampleOn
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
                                              unsafeRepeat
                                                (toInt ii + 1)
                                                ">"
                                          )
                                          arr
                                      )
                            )
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
          )
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
  liftEffect do
    b' <- window >>= document >>= body
    for_ (toElement <$> b') \elt -> do
      ffi <- makeFFIDOMSnapshot
      let
        evt = deku elt
          ( subgraph (pure (Tuple unit (InsertOrUpdate unit)))
              (const $ atariSpeaks atar (const $ pure unit))
          )
          effectfulDOMInterpret
      _ <- subscribe (toEvent evt) \i -> i ffi
      pure unit