module WAGS.Example.Docs.Subgraph.Slider where

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
  launchAff_ $ bracketCtx
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