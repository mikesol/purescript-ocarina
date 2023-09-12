module Ocarina.Example.Docs.Intro.IntroEx where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Control.Parallel (parTraverse)
import Control.Plus (empty)
import Data.Array (zip, (..))
import Data.Array.NonEmpty (fromArray, singleton)
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable (for_, oneOf, oneOfMap, traverse_)
import Data.Homogeneous.Record as Rc
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (under, unwrap)
import Data.Number (pi)
import Data.Op (Op(..))
import Data.Profunctor.Strong (second)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toNumber)
import Deku.Control (text)
import Deku.Core (Nut, useRef, useState')
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Combinators (injectElement, numberOn_)
import Deku.DOM.Listeners as DL
import Deku.DOM.Self as Self
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Pursx (pursx')
import Effect (Effect, foreachE)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Random as Random
import Effect.Ref (new, read, write)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Poll (Poll, dredge, poll)
import Foreign.Object (fromHomogeneous, values)
import Graphics.Canvas (CanvasElement, arc, beginPath, fill, fillRect, getContext2D, setFillStyle)
import Ocarina.Clock (withACTime)
import Ocarina.Control (analyser_, bandpass, delay, fan1, fix, gain, gain_, highpass, lowpass, playBuf)
import Ocarina.Core (Audible, AudioEnvelope(..), AudioNumeric(..), _linear, bangOn)
import Ocarina.Core (Node, Po2(..))
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import Ocarina.Example.Docs.Util (raceSelf)
import Ocarina.Interpret (close, constant0Hack, context, decodeAudioDataFromUri, getByteFrequencyData)
import Ocarina.Math (calcSlope)
import Ocarina.Properties as P
import Ocarina.Run (run2)
import Ocarina.WebAPI (AnalyserNodeCb(..))
import Random.LCG (mkSeed)
import Test.QuickCheck.Gen (elements, evalGen)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Px = "<section>@ex1@</section>"

buffers' =
  { pluck0: "https://freesound.org/data/previews/493/493016_10350281-lq.mp3"
  , pluck1: "https://freesound.org/data/previews/141/141524_2558140-lq.mp3"
  , strum0: "https://freesound.org/data/previews/234/234738_3635427-lq.mp3"
  --, bass: "https://freesound.org/data/previews/381/381517_7088365-lq.mp3"
  }

dgl d de g ge h he i =
  delay d de [ gain g ge [ lowpass h he i ] ]

dgh d de g ge h he i =
  delay d de [ gain g ge [ highpass h he i ] ]

dgb d de g ge h he i =
  delay d de [ gain g ge [ bandpass h he i ] ]

twoPi = 2.0 * pi :: Number

fade0 = pure
  $ P.gain
  $ AudioEnvelope { p: [ 1.0, 1.0, 0.75, 0.5, 0.75, 0.5, 0.75, 0.5, 0.25, 0.5, 0.25, 0.0 ], o: 0.0, d: 24.0 }

fade1 = pure
  $ P.gain
  $ AudioEnvelope { p: [ 1.0, 1.0, 0.75, 0.5, 0.75, 0.5, 0.75, 0.5, 0.25, 0.5, 0.25, 0.0 ], o: 0.0, d: 18.0 }

cvsx = 600
cvsxs = show cvsx <> "px"
cvsxn = Int.toNumber cvsx
cvsy = 400
cvsys = show cvsy <> "px"
cvsyn = Int.toNumber cvsy
fenv s e = pure
  $ P.frequency
  $ AudioEnvelope { p: [ s, e ], o: 0.0, d: 16.0 }

denv s e = pure
  $ P.delayTime
  $ AudioEnvelope { p: [ s, e ], o: 0.0, d: 16.0 }

ttap (o /\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }

introEx
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
introEx ccb _ ev = pursx' @"@" @Px
  { ex1: Deku.do
      setAudioContext /\ audioContext <- useState'
      audioContextRef <- useRef Nothing audioContext
      setStart /\ startE <- useState unit
      setStop /\ stopE <- useState'
      setLoading /\ loadingE <- useState'
      setSlider /\ slider <- useState'
      setCanvas /\ canvas <- useState'

      let
        sliderE = slider <#> \{ acTime, value } -> acTime /\ value

        music :: _ -> _ -> Array (Audible _ _)
        music buffer analyserE = do
          [ analyser_
              { cb:
                  ( AnalyserNodeCb
                      ( \a -> do
                          write (Just a) analyserE
                          pure (write Nothing analyserE)
                      )
                  )
              , fftSize: TTT7
              } $ pure $ fan1 (playBuf buffer (bangOn <|> (P.playbackRate <<< ttap <<< second (calcSlope 0.0 0.96 100.0 1.04)) <$> sliderE)) \b -> fix
              \g0 -> gain_ 1.0
                [ b
                , delay { maxDelayTime: 2.5, delayTime: 1.0 } ((P.delayTime <<< ttap <<< second (calcSlope 0.0 0.5 100.0 2.45)) <$> sliderE)
                    [ gain 0.4 ((P.gain <<< ttap <<< second (calcSlope 0.0 0.6 100.0 0.9)) <$> sliderE) [ b ] ]
                , dgh 0.15 empty 0.7 empty 1500.0 (fenv 1500.0 3000.0)
                    [ fix
                        \g1 -> gain 1.0 fade1
                          [ dgh 0.4 empty 0.5 empty 3000.0 (fenv 3000.0 100.0)
                              [ g0, g1 ]
                          ]
                    ]
                , dgh 0.29 ((P.delayTime <<< ttap <<< second (calcSlope 0.0 0.1 100.0 0.4)) <$> sliderE) {-(denv 0.29 0.9)-}  0.85 empty 2000.0
                    (fenv 2000.0 5000.0)
                    [ fix
                        \g1 -> gain_ 1.0
                          [ dgh 0.6 ((P.delayTime <<< ttap <<< second (calcSlope 0.0 0.8 100.0 0.3)) <$> sliderE) {-(denv 0.6 0.2)-}  0.6 empty 3500.0
                              (fenv 3500.0 100.0)
                              [ g0
                              , ( fix
                                    \g2 -> gain 1.0 fade0
                                      [ dgb 0.75 ((P.delayTime <<< ttap <<< second (calcSlope 0.0 0.9 100.0 0.1)) <$> sliderE) {-(denv 0.75 0.99)-}  0.6
                                          empty
                                          4000.0
                                          (fenv 4000.0 200.0)
                                          [ g1, g2 ]
                                      , dgb 0.75 (denv 0.75 0.2) 0.55 empty 200.0 (fenv 200.0 4000.0) [ b ]
                                      ]
                                )
                              ]
                          ]
                    ]
                ]
          ]
      D.div_
        [ D.canvas
            [ DA.width_ cvsxs
            , DA.height_ cvsys
            , DA.style_ "width: 100%;"
            , injectElement $ HTMLCanvasElement.fromElement >>> traverse_ \e -> do
                ctx <- getContext2D
                  ( ( unsafeCoerce
                        :: HTMLCanvasElement.HTMLCanvasElement -> CanvasElement
                    ) e
                  )
                setFillStyle ctx "black"
                fillRect ctx { width: cvsxn, height: cvsyn, x: 0.0, y: 0.0 }
                pure unit
            , Self.self
                ( ( \arr -> HTMLCanvasElement.fromElement >>> traverse_ \e -> do
                      ctx <- getContext2D
                        ( ( unsafeCoerce
                              :: HTMLCanvasElement.HTMLCanvasElement -> CanvasElement
                          ) e
                        )
                      setFillStyle ctx "black"
                      fillRect ctx { width: cvsxn, height: cvsyn, x: 0.0, y: 0.0 }
                      setFillStyle ctx "rgba(255,255,255,0.2)"
                      foreachE arr \({ x, y } /\ n) -> do
                        beginPath ctx
                        arc ctx { end: twoPi, radius: n * 40.0, start: 0.0, x: x * cvsxn, y: y * cvsyn, useCounterClockwise: false }
                        fill ctx
                  ) <$> canvas
                )
            ]
            []
        , D.input
            [ DA.xtypeRange
            , DA.min_ "0"
            , DA.max_ "100"
            , DA.step_ "1"
            , DA.value_ "50"
            , DA.style_ "width: 100%;"
            , numberOn_ DL.input \n ->
                audioContextRef >>= traverse_ \ctx ->
                  unwrap (withACTime ctx (Op setSlider)) n
            ]

            []
        , D.button
            [ oneOf
                [ DA.style_ "width:100%; padding:1.0rem;"
                , DL.runOn DL.click $ loadingE $> pure unit
                , DL.runOn DL.click $ stopE <#>
                    (_ *> (ccb (pure unit) *> setAudioContext Nothing *> setStart unit))
                , DL.runOn DL.click $ ((startE $> identity) <*> (pure (pure unit) <|> (map (\(SetCancel x) -> x) ev))) <#> \cncl -> do
                    cncl
                    setLoading unit
                    analyserE <- new Nothing
                    fib <- launchAff do
                      ctx <- context
                      c0h <- constant0Hack ctx
                      sounds <- Rc.fromHomogeneous <$> parTraverse (decodeAudioDataFromUri ctx) (Rc.homogeneous buffers')
                      ri <- liftEffect $ randomInt 0 50000
                      let
                        randSound = evalGen
                          ( elements
                              $ fromMaybe (singleton sounds.pluck0)
                              $ fromArray
                              $ values
                              $ fromHomogeneous sounds
                          )
                          { newSeed: mkSeed ri, size: 4 }
                      liftEffect do
                        rands <- 0 .. 127 # traverse \_ -> do
                          x <- Random.random
                          y <- Random.random
                          pure { x, y }
                        setAudioContext (Just ctx)
                        ssub <- run2 ctx (music randSound analyserE)
                        afe <- animationFrame
                        anisub <- subscribe afe.event \_ -> do
                          ae <- read analyserE
                          for_ ae \a -> do
                            frequencyData <-
                              getByteFrequencyData a
                            arr <- map (zip rands <<< map ((_ / 255.0) <<< toNumber)) (toArray frequencyData)
                            setCanvas arr
                            pure unit
                        let res = ssub *> c0h *> close ctx *> anisub *> afe.unsubscribe
                        setStop res
                        pure res
                    ccb do
                      setStart unit
                      launchAff_ $ raceSelf fib
                    pure unit
                ]
            ]
            [ text $ oneOf
                [ map (const "Turn off") stopE
                , map (const "Turn on") startE
                , map (const "Loading...") loadingE
                ]
            ]
        ]
  }
