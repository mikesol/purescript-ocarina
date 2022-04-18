module WAGS.Example.Docs.Events.Ex2 where

import Prelude

import Control.Alt ((<|>))
import Data.Exists (mkExists)
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import FRP.Event.Class (bang, biSampleOn, filterMap)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Example.Docs.Util (raceSelf)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2_)
import WAGS.Variant (injs_, prjs_)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

px =
  Proxy    :: Proxy         """<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Wags comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Wags <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in wags is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The wags interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The wags interval needs an audio context to work.</li>
    <li>The wags interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <pre><code>@wagtxt@</code></pre>

  <p></p>

  <pre><code>@txt@</code></pre>

  @ex2@

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
sli = injs_ (Proxy :: Proxy Slider)
slp = prjs_ (Proxy :: Proxy Slider)
type StartStop = Variant (start :: Unit, stop :: Effect Unit, loading :: Unit)
ssi = injs_ (Proxy :: Proxy StartStop)
ssp = prjs_ (Proxy :: Proxy StartStop)
start = uii.startStop (ssi.start unit)
loading = uii.startStop (ssi.loading unit)
stop r = uii.startStop (ssi.stop r)
type UIEvents = Variant (startStop :: StartStop, slider :: Slider)
uii = injs_ (Proxy :: Proxy UIEvents)

uip = prjs_ (Proxy :: Proxy UIEvents)

atari :: String
atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"

ex2
  :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
ex2 ccb _ ev = makePursx' (Proxy :: _ "@") px
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
  , ex2: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push event -> -- here
            do
              let
                ss = bang (ssi.start unit) <|> filterMap uip.startStop event
                startE = filterMap ssp.start ss
                stopE = filterMap ssp.stop ss
                sl = filterMap uip.slider event
                sl0 = filterMap slp.s0 sl
                sl1 = filterMap slp.s1 sl
                sl2 = filterMap slp.s2 sl
                -- ugh, problem is that if we are in the land of run
                -- then we need to be using Event
                -- but we are inheriting something of type Event
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
                        [ text $ oneOf [map (const "Turn off") stopE, map (const "Turn on") startE]
                        ]
                    ]
      )
  }