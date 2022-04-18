module WAGS.Example.Docs.Events.Ex1 where

import Prelude

import Control.Alt ((<|>))
import Control.Alt (alt, (<|>))
import Control.Plus (class Plus)
import Data.Array.NonEmpty ((..))
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Filterable (partitionMap)
import Data.Int (toNumber)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Profunctor (lcmap)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.Control (text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Deku.Toplevel ((🚀))
import Effect (Effect)
import Effect (Effect)
import FRP.Event (class IsEvent, Event, subscribe)
import FRP.Event.Class (bang)
import FRP.Event.Class (bang, biSampleOn)
import Math (pow)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, gain, sinOsc, (~))
import WAGS.Control (microphone, recorder, singleton, speaker2)
import WAGS.Core (AudioInput)
import WAGS.Core (ai)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Example.Docs.Util (WrapperStates(..), clickCb, mkWrapperEvent)
import WAGS.Interpret (close, context, contextState, effectfulAudioInterpret, getMicrophoneAndCamera, makeFFIAudioSnapshot, mediaRecorderToUrl)
import WAGS.Parameter (AudioEnvelope(..), AudioOnOff(..), _off, _on, apOff, apOn, dt)
import WAGS.Properties (onOff)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserMicrophone, MediaRecorderCb(..))

px =
  Proxy    :: Proxy   """<section>
  <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>
  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a pureOn event that turns it on. On and off states can be controlled with events as well, as we'll see in the example below.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

  <p>Unlike the previous examples, this one and all subsequent ones are "batteries included", meaning they are single-file, self-contained PureScript examples that you can compile and run yourself.</p>

</section>
"""


type ExampleStates = Either UIEvents WrapperStates

scene
  :: forall payload
   . BrowserMicrophone
  -> MediaRecorderCb
  -> AudioInput D2 "" () Event payload
scene m cb =
  singleton
    (recorder cb (microphone m))


data UIEvents = Init | Start | Stop (Effect Unit)

oon o = bang $ onOff $ AudioOnOff { n: _on, o }
oof o = bang $ onOff $ AudioOnOff { n: _off, o }
cell = lcmap toNumber \i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 `pow` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 `pow` i)))
    genv x = bang $ P.gain
      $ AudioEnvelope
          { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
          , o: x + 0.3 * (i * (1.005 `pow` i))
          , d: 0.8
          }
    strand x y =
      gain 0.0 (genv x) (sinOsc (200.0 + i * y) (ooo' x))
  ( strand 0.2 4.0
      ~ strand 0.3 6.0
      ~ strand 0.45 14.0
      ~ strand 0.7 20.0
  )

txt :: String
txt = """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Exists (mkExists)
import Data.Foldable (for_, oneOfMap)
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
            ss = filterMap uip.startStop event
            sl = filterMap uip.slider event
            sl0 = filterMap slp.s0 sl
            sl1 = filterMap slp.s1 sl
            sl2 = filterMap slp.s2 sl
            music = run2_ $ loopBuf buffer
              ( pureOn
                  <|> playbackRate <$> sl0
                  <|> loopStart <$> sl1
                  <|> loopEnd <$> biSampleOn sl2
                    (add <$> (bang 0.0 <|> sl1))
              )
          D.div_
            $
              map
                ( \{ mn, mx, f } -> D.input
                    ( oneOfMap bang
                        [ D.Xtype := "range"
                        , D.Max := mx
                        , D.Min := mn
                        , D.OnInput := cb \e -> for_
                            ( target e
                                >>= fromEventTarget
                            )
                            ( valueAsNumber
                                >=> push <<< uii.slider <<< f
                            )
                        ]
                    )
                    []
                )
                [ { mn: "0.5", mx: "5.0", f: sli.s0 }
                , { mn: "0.0", mx: "1.0", f: sli.s1 }
                , { mn: "0.01", mx: "1.0", f: sli.s2 }
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
      }
"""

ex1
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
ex1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { txt: nut (text_ txt)
  , ex1: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push -> lcmap (alt (bang Init)) \event -> -- here
              D.div_
                [ D.button
                    ( (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map Tuple event)) <#> -- here
                        \(e /\ c) -> D.OnClick := cb -- here
                          ( const $ case e of
                              Stop u -> u *> push Start *> ccb (pure unit) -- here
                              _ -> do
                                c -- here
                                r <- run2_ $ gain_ 1.0
                                  $ ai $ 0 .. 100 <#> cell
                                ccb (r *> push Start) -- here
                                push $ Stop r
                          )
                    )
                    [ text $ event <#> case _ of
                        Stop _ -> "Turn off"
                        _ -> "Turn on"
                    ]
                ]
      )
  }
