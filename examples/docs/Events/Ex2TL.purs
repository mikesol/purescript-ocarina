module WAGS.Example.Docs.Events.Ex2TL where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOfMap, traverse_)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Control (blank, text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (behavior, sampleBy)
import FRP.Event (Event, bus, makeEvent, memoize, subscribe)
import FRP.Event.Class (bang, filterMap)
import Type.Proxy (Proxy(..))
import WAGS.Clock (interval)
import WAGS.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import WAGS.Core (Node, mix)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioEnvelope(..), bangOn)
import WAGS.Properties (frequency)
import WAGS.Properties as P
import WAGS.Run (run2e)
import WAGS.Variant (injs_, prjs_)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type StartStop = Variant (start :: Unit, stop :: Effect Unit)
ssi = injs_ (Proxy :: _ StartStop)
start = uii.startStop (ssi.start unit)
stop r = uii.startStop (ssi.stop r)

type UIEvents = Variant
  ( init :: Unit
  , startStop :: StartStop
  , slider :: Number
  )

random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

uii = injs_ (Proxy :: _ UIEvents)
uip = prjs_ (Proxy :: _ UIEvents)

-- pentatonic scale
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

main :: Effect Unit
main = runInBody1
  ( bus \push -> lcmap (bang (uii.init unit) <|> _) \event -> do
      let
        ss = bang (ssi.start unit) <|> filterMap uip.startStop event
        sl = filterMap uip.slider event
        music :: forall lock. _ -> Event (Array (Node _ lock _))
        music evt' = memoize evt' \evt -> do
          let
            pitch = map fst evt
            -- to avoid artifacts in the pitch change
            time = map (add 0.01 <<< snd) evt
            e0 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e1 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.3, 0.1, 0.05, 0.01, 0.005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e2 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.15, 0.05, 0.01, 0.005, 0.0005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            f0 = bangOn <|> frequency <<< cp <$> pitch
          [ fan1 (triangleOsc 0.0 f0) \ipt _ -> do
              mix $ gain_ 2.0
                [ gain 0.0 (P.gain <$> e0)
                    [ bandpass_
                        { frequency: 1000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e1)
                    [ bandpass_
                        { frequency: 2000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e2)
                    [ highpass_
                        { frequency: 4000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                ]
          ]
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
                blank
            ]
        , D.button
            ( ss <#>
                \e -> D.OnClick := cb
                  ( const $ e # match
                      { stop: \u -> u *>
                          push start
                      , start: \_ -> do
                          ctx <- context
                          let
                            myIvl = sampleBy Tuple random
                              $ interval ctx 0.91
                              $ map (calcSlope 0.0 0.42 100.0 1.4) sl
                          r <- run2e ctx (music myIvl)
                          push (stop (r *> close ctx))
                      }
                  )
            )
            [ text $
                match
                  { stop: \_ -> "Turn off"
                  , start: \_ -> "Turn on"
                  } <$> ss
            ]
        ]
  )