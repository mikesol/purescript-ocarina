module WAGS.Example.Docs.Subgraph.SliderEx where

import Prelude

import Control.Alt ((<|>))
import Data.Exists (mkExists)
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Hashable (class Hashable)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
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
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (behavior, sampleBy)
import FRP.Event (Event, fold, keepLatest, makeEvent, subscribe)
import FRP.Event.Class (bang)
import FRP.Event.Class (bang, biSampleOn)
import FRP.Event.Class (bang, biSampleOn, filterMap)
import FRP.Event.Time (delay)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf, playBuf)
import WAGS.Core (Node, mkSubgraph)
import WAGS.Core as C
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Example.Docs.Util (raceSelf)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Parameter (bangOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2_)
import WAGS.Variant (injs_, prjs_)
import WAGS.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

px =
  Proxy    :: Proxy         """<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Wags bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>WAGS.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Wags parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
"""

txt :: String
txt =
  """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Exists (mkExists)
import Data.Foldable (oneOf, oneOfMap)
import Data.Hashable (class Hashable)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (fst)
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
import Effect.Random as Random
import FRP.Behavior (behavior, sampleBy)
import FRP.Event (fold, makeEvent, subscribe)
import FRP.Event.Class (bang, filterMap, keepLatest)
import FRP.Event.Time (delay)
import Type.Proxy (Proxy(..))
import WAGS.Control (playBuf)
import WAGS.Core (mkSubgraph, subgraph)
import WAGS.Core as C
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Parameter (bangOn)
import WAGS.Run (run2_)
import WAGS.Variant (injs_, prjs_)
import WAGS.WebAPI (BrowserAudioBuffer)

type StartStop = Variant (start :: Unit, stop :: Effect Unit)
ssi = injs_ (Proxy :: _ StartStop)
start = uii.startStop (ssi.start unit)
stop r = uii.startStop (ssi.stop r)
ssp = prjs_ (Proxy :: _ StartStop)
type UIEvents = Variant (startStop :: StartStop, slider :: Unit)
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

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"

random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

main :: Effect Unit
main = do
  { push } <- loading 🚆 go
  launchAff_ $ ctxAff
    \ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
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
            sl = sampleBy (/\) random
              $ fold (\_ b -> b + 1) (filterMap uip.slider event) 0
            music = run2_
              [ subgraph
                  ( keepLatest $ map
                      (\i ->
                          oneOf
                            [ bang $ i /\ C.Insert
                            , delay 5000 $ bang $ i /\ C.Remove
                            ]
                      )
                      sl
                  )
                  ( \i -> mkSubgraph
                      ( playBuf
                          { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                          bangOn
                      )
                  )
              ]
          D.div_
            [ D.div_
                [ text_ "Slide me!"
                , D.input
                    ( oneOfMap bang
                        [ D.Xtype := "range"
                        , D.Min := "0"
                        , D.Max := "100"
                        , D.Step := "1"
                        , D.Value := "50"
                        , D.OnInput := cb (const (push $ uii.slider unit))
                        ]
                    )
                    []
                ]
            , D.button
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
      }
"""

type StartStop = Variant (start :: Unit, stop :: Effect Unit, loading :: Unit)
ssi = injs_ (Proxy :: Proxy StartStop)
ssp = prjs_ (Proxy :: Proxy StartStop)
start = uii.startStop (ssi.start unit)
loading = uii.startStop (ssi.loading unit)
stop r = uii.startStop (ssi.stop r)
type UIEvents = Variant (startStop :: StartStop, slider :: Unit)
uii = injs_ (Proxy :: _ UIEvents)
uip = prjs_ (Proxy :: _ UIEvents)

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"

random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

ex1
  :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
ex1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { wagtxt: nut
      ( text_
          """run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ oneOf
      [ bangOn
      , (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      , (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      , (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (bang 0.0 <|> sl1))
      ]"""
      )
  , txt: nut (text_ txt)
  , ex1: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push event -> -- here

            do
              let
                ss = bang (ssi.start unit) <|> filterMap uip.startStop event
                startE = filterMap ssp.start ss
                stopE = filterMap ssp.stop ss
                sl = sampleBy (/\) random
                  $ fold (\_ b -> b + 1) (filterMap uip.slider event) 0

                music :: forall lock. _ -> Array (Node _ lock _ _)
                music buffer =
                  [ C.subgraph
                      ( keepLatest $ map
                          ( \i ->
                              oneOf
                                [ bang $ i /\ C.Insert
                                , delay 5000 $ bang $ i /\ C.Remove
                                ]
                          )
                          sl
                      )
                      ( \i -> mkSubgraph
                          ( playBuf
                              { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                              bangOn
                          )
                      )
                  ]
              D.div_
                [ D.div_
                    [ text_ "Slide me!"
                    , D.input
                        ( oneOfMap bang
                            [ D.Xtype := "range"
                            , D.Min := "0"
                            , D.Max := "100"
                            , D.Step := "1"
                            , D.Value := "50"
                            , D.OnInput := cb (const (push $ uii.slider unit))
                            ]
                        )
                        []
                    ]
                , D.button
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
                                    buffer <- ctxAff \ctx -> decodeAudioDataFromUri ctx bell
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
                    [ text $ oneOf [ map (const "Turn off") stopE, map (const "Turn on") startE ]
                    ]
                ]
      )
  }
