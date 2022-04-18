module WAGS.Example.Docs.Events.Ex1TL where

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

atari = "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"

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
          let ss = filterMap uip.startStop event
          let sl = filterMap uip.slider event
          let sl0 = filterMap slp.s0 sl
          let sl1 = filterMap slp.s1 sl
          let sl2 = filterMap slp.s2 sl
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
                                  push (uii.startStop (ssi.start unit))
                              , start: \_ -> do
                                  r <- run2_ $ loopBuf buffer
                                    (pureOn
                                        <|> playbackRate <$> sl0
                                        <|> loopStart <$> sl1
                                        <|> loopEnd <$> biSampleOn sl2
                                          (add <$> (bang 0.0 <|> sl1))
                                    )
                                  push (uii.startStop (ssi.stop r))
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