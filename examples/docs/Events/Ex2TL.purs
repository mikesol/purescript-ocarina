module WAGS.Example.Docs.Events.Ex2TL where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (oneOfMap, traverse_)
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Toplevel ((🚀))
import Effect (Effect)
import FRP.Event.Class (bang, filterMap)
import FRP.Event.Memoize (memoize)
import Type.Proxy (Proxy(..))
import WAGS.Clock (interval)
import WAGS.Control (triangleOsc)
import WAGS.Interpret (context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (bangOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2, run2_)
import WAGS.Variant (injs_, prjs_)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type StartStop = Variant (start :: Unit, stop :: Effect Unit)
ssi = injs_ (Proxy :: _ StartStop)
start = uii.startStop (ssi.start unit)
stop r = uii.startStop (ssi.stop r)
ssp = prjs_ (Proxy :: _ StartStop)

type UIEvents = Variant
  ( init :: Unit
  , startStop :: StartStop
  , slider :: Number
  )

uii = injs_ (Proxy :: _ UIEvents)
uip = prjs_ (Proxy :: _ UIEvents)

main :: Effect Unit
main = uii.init unit 🚀 \push event -> do
  let
    ss = bang (ssi.start unit) <|> filterMap uip.startStop event
    sl = filterMap uip.slider event
    music evt = triangleOsc 440.0 empty
  D.div_
    [ D.div_
        [ text_ "tempo"
        , D.input
            ( oneOfMap bang
                [ D.Xtype := "range"
                , D.Min := "0"
                , D.Max := "100"
                , D.Step := "1"
                , D.Value := "50"
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
        ]
    , D.button
        ( ss <#>
            \e -> D.OnClick := cb
              ( const $ e # match
                  { stop: \u -> u *>
                      push start
                  , start: \_ -> do
                      ctx <- context
                      myIvl <- memoize (interval ctx 0.5 sl)
                      r <- run2 ctx (music myIvl)
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