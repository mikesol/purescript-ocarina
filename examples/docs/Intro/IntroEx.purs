module WAGS.Example.Docs.Intro.IntroEx where

import Prelude

import Control.Alt ((<|>))
import Control.Parallel (parTraverse)
import Control.Plus (empty)
import Data.Array (zip, (..))
import Data.Array.NonEmpty (fromArray, fromNonEmpty, singleton)
import Data.ArrayBuffer.Typed (toArray)
import Data.Exists (mkExists)
import Data.Foldable (for_, oneOf, oneOfMap, traverse_)
import Data.Homogeneous.Record as Rc
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toInt, toNumber)
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.Control (text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.Core (SubgraphF(..))
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Deku.Subgraph (subgraph, (@@))
import Deku.Subgraph as Sg
import Effect (Effect, foreachE)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (info)
import Effect.Random (randomInt)
import Effect.Random as Random
import Effect.Ref (new, read, write)
import FRP.Behavior (behavior, sampleBy)
import FRP.Event (Event, filterMap, fold, keepLatest, makeEvent, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang)
import FRP.Event.Class (bang, biSampleOn)
import FRP.Event.Class (bang, biSampleOn, filterMap)
import Foreign.Object (fromHomogeneous, values)
import Graphics.Canvas (arc, beginPath, fill, fillRect, setFillStyle)
import Math (pi)
import Random.LCG (mkSeed)
import Test.QuickCheck.Gen (elements, evalGen)
import Type.Proxy (Proxy(..))
import WAGS.Control (analyser_, bandpass, delay, gain, gain_, highpass, highpass_, loopBuf, lowpass, playBuf)
import WAGS.Core (Node, Po2(..), fan, fix, mkSubgraph)
import WAGS.Core as C
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Example.Docs.Util (raceSelf)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri, getByteFrequencyData)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioEnvelope(..), bangOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Properties as P
import WAGS.Run (run2_)
import WAGS.Variant (injs_, prjs_)
import WAGS.WebAPI (AnalyserNodeCb(..), BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

px = Proxy :: Proxy """<section>@ex1@</section>"""

type StartStop = Variant (start :: Unit, stop :: Effect Unit, loading :: Unit)
ssi = injs_ (Proxy :: Proxy StartStop)
ssp = prjs_ (Proxy :: Proxy StartStop)
start = uii.startStop (ssi.start unit)
loading = uii.startStop (ssi.loading unit)
stop r = uii.startStop (ssi.stop r)
type CanvasInfo = { x :: Number, y :: Number } /\ Number
type UIEvents = Variant
  ( startStop :: StartStop
  , slider :: Number
  , canvas :: Array CanvasInfo
  )

uii = injs_ (Proxy :: _ UIEvents)
uip = prjs_ (Proxy :: _ UIEvents)

buffers' =
  { pluck0: "https://freesound.org/data/previews/493/493016_10350281-lq.mp3"
  , pluck1: "https://freesound.org/data/previews/141/141524_2558140-lq.mp3"
  , strum0: "https://freesound.org/data/previews/234/234738_3635427-lq.mp3"
  --, bass: "https://freesound.org/data/previews/381/381517_7088365-lq.mp3"
  }

random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

dgl d de g ge h he i =
  delay d de [ gain g ge [ lowpass h he i ] ]

dgh d de g ge h he i =
  delay d de [ gain g ge [ highpass h he i ] ]

dgb d de g ge h he i =
  delay d de [ gain g ge [ bandpass h he i ] ]

twoPi = 2.0 * pi :: Number

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [ 1.0, 1.0, 0.75, 0.5, 0.75, 0.5, 0.75, 0.5, 0.25, 0.5, 0.25, 0.0 ], o: 0.0, d: 24.0 }

fade1 = bang
  $ P.gain
  $ AudioEnvelope { p: [ 1.0, 1.0, 0.75, 0.5, 0.75, 0.5, 0.75, 0.5, 0.25, 0.5, 0.25, 0.0 ], o: 0.0, d: 18.0 }

cvsx = 600
cvsxs = show cvsx <> "px"
cvsxn = Int.toNumber cvsx
cvsy = 400
cvsys = show cvsy <> "px"
cvsyn = Int.toNumber cvsy
fenv s e = bang
  $ P.frequency
  $ AudioEnvelope { p: [ s, e ], o: 0.0, d: 16.0 }

denv s e = bang
  $ P.delayTime
  $ AudioEnvelope { p: [ s, e ], o: 0.0, d: 16.0 }

introEx
  :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
introEx ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ex1: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push event -> -- here

            do
              let
                ss = bang (ssi.start unit) <|> filterMap uip.startStop event
                sliderE = filterMap uip.slider event
                startE = filterMap ssp.start ss
                loadingE = filterMap ssp.loading ss
                stopE = filterMap ssp.stop ss

                music :: forall lock. _ -> _ -> Array (Node _ lock _ _)
                music buffer analyserE =
                  [ analyser_
                      { cb:
                          ( AnalyserNodeCb
                              ( \a -> do
                                  write (Just a) analyserE
                                  pure (write Nothing analyserE)
                              )
                          )
                      , fftSize: TTT7
                      } $ fan (playBuf buffer (bangOn <|> (P.playbackRate <<< calcSlope 0.0 0.96 100.0 1.04) <$> sliderE)) \b -> fix
                      \g0 -> gain_ 1.0
                        [ b
                        , dgh 0.15 empty 0.7 empty 1500.0 (fenv 1500.0 3000.0)
                            [ fix
                                \g1 -> gain 1.0 fade1
                                  [ dgh 0.4 empty 0.5 empty 3000.0 (fenv 3000.0 100.0)
                                      [ g0, g1 ]
                                  ]
                            ]
                        , dgh 0.29 ((P.delayTime <<< calcSlope 0.0 0.1 100.0 0.4) <$> sliderE) {-(denv 0.29 0.9)-}  0.85 empty 2000.0 (fenv 2000.0 5000.0)
                            [ fix
                                \g1 -> gain_ 1.0
                                  [ dgh 0.6 ((P.delayTime <<< calcSlope 0.0 0.8 100.0 0.3) <$> sliderE) {-(denv 0.6 0.2)-}  0.6 empty 3500.0 (fenv 3500.0 100.0)
                                      [ g0
                                      , ( fix
                                            \g2 -> gain 1.0 fade0
                                              [ dgb 0.75 ((P.delayTime <<< calcSlope 0.0 0.9 100.0 0.1) <$> sliderE) {-(denv 0.75 0.99)-}  0.6 empty 4000.0
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
                    ( oneOfMap bang
                        [ D.Width := cvsxs
                        , D.Height := cvsys
                        , D.Style := "width: 100%;"
                        , D.Draw2D := \ctx -> do
                            setFillStyle ctx "black"
                            fillRect ctx { width: cvsxn, height: cvsyn, x: 0.0, y: 0.0 }
                            pure unit
                        ] <|>
                        ( ( \arr -> D.Draw2D := \ctx -> do
                              setFillStyle ctx "black"
                              fillRect ctx { width: cvsxn, height: cvsyn, x: 0.0, y: 0.0 }
                              setFillStyle ctx "rgba(255,255,255,0.2)"
                              foreachE arr \({ x, y } /\ n) -> do
                                beginPath ctx
                                arc ctx { end: twoPi, radius: n * 40.0, start: 0.0, x: x * cvsxn, y: y * cvsyn }
                                fill ctx
                          ) <$> filterMap uip.canvas event
                        )
                    )
                    []
                , D.input
                    ( oneOfMap bang
                        [ D.Xtype := "range"
                        , D.Min := "0"
                        , D.Max := "100"
                        , D.Step := "1"
                        , D.Value := "50"
                        , D.Style := "width: 100%;"
                        , D.OnInput := cb
                            ( traverse_
                                ( valueAsNumber
                                    >=> push <<< uii.slider
                                )
                                <<< (=<<) fromEventTarget
                                <<< target
                            )
                        ]
                    )
                    []
                , D.button
                    ( oneOf
                        [ bang $ D.Style := "width:100%; padding:1.0rem;"
                        , (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map Tuple ss)) <#>
                            \(e /\ cncl) -> D.OnClick := cb
                              ( const $ e # match
                                  { loading: \_ -> pure unit
                                  , stop: \u -> u
                                      *> ccb (pure unit)
                                      *> push start
                                  , start: \_ -> do
                                      cncl
                                      push loading
                                      afe <- animationFrameEvent
                                      analyserE <- new Nothing
                                      fib <- launchAff do
                                        sounds <- Rc.fromHomogeneous <$> ctxAff \ctx -> parTraverse (decodeAudioDataFromUri ctx) (Rc.homogeneous buffers')
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
                                          ssub <- run2_ (music randSound analyserE)
                                          anisub <- subscribe afe \_ -> do
                                            ae <- read analyserE
                                            for_ ae \a -> do
                                              frequencyData <-
                                                getByteFrequencyData a
                                              arr <- map (zip rands <<< map ((_ / 255.0) <<< toNumber)) (toArray frequencyData)
                                              push (uii.canvas arr)
                                              pure unit
                                          let res = ssub *> anisub
                                          push (stop res)
                                          pure res
                                      ccb do
                                        push start
                                        launchAff_ $ raceSelf fib
                                      pure unit
                                  }
                              )
                        ]
                    )
                    [ text $ oneOf
                        [ map (const "Turn off") stopE
                        , map (const "Turn on") startE
                        , map (const "Loading...") loadingE
                        ]
                    ]
                ]
      )
  }