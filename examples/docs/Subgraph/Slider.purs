module WAGS.Example.Docs.Subgraph.Slider where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Control (blank, plant, switcher, text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (bus, create, fold, makeEvent, subscribe)
import FRP.Event.Class (bang, filterMap, keepLatest)
import FRP.Event.Time (delay)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, playBuf)
import WAGS.Core (StreamingAudio(..))
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
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


bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"
  :: String

random :: Behavior Number
random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push loading
  launchAff_ $ bracketCtx
    \ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
      <<< push
      <<< State
      <<< sti.loaded
  where
  scene :: forall lock payload. State -> Element lock payload
  scene = unwrap >>> match
    { loaded: \buffer -> D.div_ $ keepLatest $ bus \push event -> do
        let
          ss = bang (ssi.start unit) <|> filterMap uip.startStop event
          sl = sampleBy (/\) random
            $ fold (\_ b -> b + 1) (filterMap uip.slider event) 0
          music = run2_
            [ gain_ 1.0 $ map
                    ( \i ->
                        oneOf
                          [ bang $ Sound $ playBuf
                              { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                              bangOn
                          , delay 5000 $ bang $ Silence
                          ]
                    )
                    sl
            ]
        plant
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
                  blank
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
    , loading: \_ -> D.div_ [ text_ "Loading..." ]
    }