module WAGS.Example.Docs.Events.Ex1 where

import Prelude

import Control.Alt ((<|>))
import Control.Alt ((<|>))
import Control.Alt (alt, (<|>))
import Control.Plus (class Plus)
import Data.Array.NonEmpty ((..))
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Exists (mkExists)
import Data.Filterable (partitionMap)
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Hashable (class Hashable)
import Data.Int (toNumber)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (lcmap)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Attribute (cb, (:=))
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.Control (text, text_)
import Deku.Control (text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.Core (SubgraphF(..))
import Deku.DOM as D
import Deku.DOM as D
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Deku.Subgraph ((@@))
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Deku.Subgraph as Sg
import Deku.Toplevel ((🚀))
import Deku.Toplevel ((🚆))
import Effect (Effect)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (class IsEvent)
import FRP.Event.Class (bang)
import FRP.Event.Class (bang, biSampleOn)
import FRP.Event.Class (bang, biSampleOn, filterMap, keepLatest)
import Math (pow)
import Type.Proxy (Proxy(..))
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, gain, sinOsc, (~))
import WAGS.Control (loopBuf)
import WAGS.Control (microphone, recorder, singleton, speaker2)
import WAGS.Core (AudioInput, Node)
import WAGS.Core (ai)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Example.Docs.Util (WrapperStates(..), clickCb, mkWrapperEvent, raceSelf)
import WAGS.Interpret (close, context, contextState, effectfulAudioInterpret, getMicrophoneAndCamera, makeFFIAudioSnapshot, mediaRecorderToUrl)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Parameter (AudioEnvelope(..), AudioOnOff(..), _off, _on, apOff, apOn, dt)
import WAGS.Parameter (pureOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Properties (onOff)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)
import WAGS.Run (run2_)
import WAGS.Variant (injs_, prjs_)
import WAGS.WebAPI (BrowserAudioBuffer)
import WAGS.WebAPI (BrowserMicrophone, MediaRecorderCb(..))
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

px =
  Proxy    :: Proxy         """<section>
  <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Wags bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>pureOn</code> event that turns it on.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>WAGS.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Wags parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

  <p>Unlike the previous examples, this one and all subsequent ones are "batteries included", meaning they are single-file, self-contained PureScript examples that you can compile and run yourself.</p>

</section>
"""

txt :: String
txt =
  """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Exists (mkExists)
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Hashable (class Hashable)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (SubgraphF(..))
import Deku.DOM as D
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Deku.Toplevel ((🚆))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event.Class (bang, biSampleOn, filterMap, keepLatest)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2_)
import WAGS.Variant (injs_, prjs_)
import WAGS.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Slider = Variant (s0 :: Number, s1 :: Number, s2 :: Number)
sli = injs_ (Proxy :: _ Slider)
slp = prjs_ (Proxy :: _ Slider)
type StartStop = Variant (start :: Unit, stop :: Effect Unit)
ssi = injs_ (Proxy :: _ StartStop)
start = uii.startStop (ssi.start unit)
stop r = uii.startStop (ssi.stop r)
ssp = prjs_ (Proxy :: _ StartStop)
type UIEvents = Variant (startStop :: StartStop, slider :: Slider)
uii = injs_ (Proxy :: _ UIEvents)
uip = prjs_ (Proxy :: _ UIEvents)
type State' = Variant (loading :: Unit, loaded :: BrowserAudioBuffer)

derive instance Newtype State _
newtype State = State State'
sti = injs_ (Proxy :: _ State')
loading = State $ sti.loading unit
stp = prjs_ (Proxy :: _ State')

derive instance Eq State

instance Hashable State where
  hash (State v) = match { loading: \_ -> 0, loaded: \_ -> 1 } v

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"

main :: Effect Unit
main = do
  { push } <- loading 🚆 go
  launchAff_ $ ctxAff
    \ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< State
      <<< sti.loaded
  where
  go _ ev =
    ( keepLatest $
        ( \i@(State i') -> match
            { loading: \_ -> bang (i /\ Sg.Insert)
            , loaded: \_ -> bang (loading /\ Sg.Remove)
                <|> bang (i /\ Sg.Insert)
            }
            i'
        ) <$> ev
    ) @@ scene
    where
    scene = unwrap >>> match
      { loaded: \buffer -> mkExists $ SubgraphF \push event -> do
          let
            ss = bang (ssi.start unit) <|> filterMap uip.startStop event
            sl = filterMap uip.slider event
            sl0 = filterMap slp.s0 sl
            sl1 = filterMap slp.s1 sl
            sl2 = filterMap slp.s2 sl
            music = run2_
              $ loopBuf buffer
              $ oneOf
                  [ pureOn
                  , playbackRate <$> sl0
                  , loopStart <$> sl1
                  , loopEnd <$> biSampleOn sl2
                      (add <$> (bang 0.0 <|> sl1))
                  ]
          D.div_
            $
              map
                ( \{ l, mn, mx, f } -> D.div_
                    [ text_ l
                    , D.input
                        ( oneOfMap bang
                            [ D.Xtype := "range"
                            , D.Min := mn
                            , D.Max := mx
                            , D.OnInput := cb
                                ( traverse_
                                    ( valueAsNumber
                                        >=> push <<< uii.slider <<< f
                                    )
                                    <<< (=<<) fromEventTarget
                                    <<< target
                                )
                            ]
                        )
                        []
                    ]
                )
                [ { l: "Playback rate", mn: "0.5", mx: "5.0", f: sli.s0 }
                , { l: "Loop start", mn: "0.0", mx: "1.0", f: sli.s1 }
                , { l: "Loop end", mn: "0.01", mx: "1.0", f: sli.s2 }
                ] <>
                [ D.button
                    ( ss <#>
                        \e -> D.OnClick := cb
                          ( const $ e # match
                              { stop: \u -> u *>
                                  push start
                              , start: \_ -> do
                                  r <- music
                                  push (stop r)
                              }
                          )
                    )
                    [ text $ ss <#> match
                        { stop: \_ -> "Turn off"
                        , start: \_ -> "Turn on"
                        }
                    ]
                ]
      , loading: \_ -> mkExists
          $ SubgraphF \_ _ -> D.div_ [ text_ "Loading..." ]
      }"""

type Slider = Variant (s0 :: Number, s1 :: Number, s2 :: Number)
sli = injs_ (Proxy :: _ Slider)
slp = prjs_ (Proxy :: _ Slider)
type StartStop = Variant (start :: Unit, stop :: Effect Unit, loading :: Unit)
ssi = injs_ (Proxy :: _ StartStop)
ssp = prjs_ (Proxy :: _ StartStop)
start = uii.startStop (ssi.start unit)
loading = uii.startStop (ssi.loading unit)
stop r = uii.startStop (ssi.stop r)
type UIEvents = Variant (startStop :: StartStop, slider :: Slider)
uii = injs_ (Proxy :: _ UIEvents)
uip = prjs_ (Proxy :: _ UIEvents)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"

ex1
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
ex1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { wagtxt: nut
      ( text_
          """run2_
  $ loopBuf buffer
  $ oneOf
      [ pureOn
      , playbackRate <$> sl0
      , loopStart <$> sl1
      , loopEnd <$> biSampleOn sl2
          (add <$> (bang 0.0 <|> sl1))
      ]"""
      )
  , txt: nut (text_ txt)
  , ex1: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push (event :: event UIEvents) -> -- here
            do
              let
                ss = bang (ssi.start unit) <|> filterMap uip.startStop event
                sl = filterMap uip.slider event
                sl0 = filterMap slp.s0 sl
                sl1 = filterMap slp.s1 sl
                sl2 = filterMap slp.s2 sl
                -- ugh, problem is that if we are in the land of run
                -- then we need to be using Event
                -- but we are inheriting something of type Event
                music :: BrowserAudioBuffer -> Node D2 "" () event payload
                music buffer = loopBuf buffer
                  $ oneOf
                      [ pureOn
                      , playbackRate <$> sl0
                      , loopStart <$> sl1
                      , loopEnd <$> biSampleOn sl2
                          (add <$> (bang 0.0 <|> sl1))
                      ]
              D.div_
                $
                  map
                    ( \{ l, mn, mx, f } -> D.div_
                        [ text_ l
                        , D.input
                            ( oneOfMap bang
                                [ D.Xtype := "range"
                                , D.Min := mn
                                , D.Max := mx
                                , D.OnInput := cb
                                    ( traverse_
                                        ( valueAsNumber
                                            >=> push <<< uii.slider <<< f
                                        )
                                        <<< (=<<) fromEventTarget
                                        <<< target
                                    )
                                ]
                            )
                            []
                        ]
                    )
                    [ { l: "Playback rate", mn: "0.5", mx: "5.0", f: sli.s0 }
                    , { l: "Loop start", mn: "0.0", mx: "1.0", f: sli.s1 }
                    , { l: "Loop end", mn: "0.01", mx: "1.0", f: sli.s2 }
                    ] <>
                    [ D.button
                        ( (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map Tuple ss)) <#>
                            \(e /\ cncl) -> D.OnClick := cb
                              ( const $ e # match
                                  { loading: \_ -> pure unit
                                  , stop: \u -> u
                                      *> ccb (pure unit)
                                      *> push start
                                  , start: \_ -> do
                                      cncl
                                      push loading
                                      fib <- launchAff do
                                        buffer <- ctxAff \ctx -> decodeAudioDataFromUri ctx atari
                                        liftEffect do
                                          res <- run2_ (music buffer)
                                          push (stop res)
                                          pure res
                                      ccb do
                                        push start
                                        launchAff_ $ raceSelf fib
                                      pure unit
                                  }
                              )
                        )
                        [ text $ ss <#> match
                            { stop: \_ -> "Turn off"
                            , start: \_ -> "Turn on"
                            }
                        ]
                    ]
      -- D.div_
      --   [ D.button
      --       ( (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map Tuple event)) <#> -- here
      --           \(e /\ c) -> D.OnClick := cb -- here
      --             ( const $ case e of
      --                 Stop u -> u *> push Start *> ccb (pure unit) -- here
      --                 _ -> do
      --                   c -- here
      --                   r <- run2_ $ gain_ 1.0
      --                     $ ai $ 0 .. 100 <#> cell
      --                   ccb (r *> push Start) -- here
      --                   push $ Stop r
      --             )
      --       )
      --       [ text $ event <#> case _ of
      --           Stop _ -> "Turn off"
      --           _ -> "Turn on"
      --       ]
      --   ]
      )
  }
