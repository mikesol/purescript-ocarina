module WAGS.Example.Docs.Events.Ex1TL where

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
      }