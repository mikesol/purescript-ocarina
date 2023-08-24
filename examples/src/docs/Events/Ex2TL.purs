module Ocarina.Example.Docs.Events.Ex2TL where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Data.Foldable (oneOf, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Random as Random
import FRP.Event (makeEvent, subscribe)
import FRP.Poll (Poll, dredge, poll, rant)
import Ocarina.Clock (interval)
import Ocarina.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), bangOn)
import Ocarina.Interpret (close, context)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (frequency)
import Ocarina.Properties as P
import Ocarina.Run (run2)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

random :: Poll Number
random = poll \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

-- pentatonic scale
cp :: Number -> Number
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

main :: Effect Unit
main = runInBody Deku.do
  setStart /\ start <- useState unit
  setStop /\ stop <- useState'
  setSlider /\ slider <- useState'

  let
    music :: _ -> Array (Audible _ _)
    music evt = do
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
      [ fan1 (triangleOsc 0.0 f0) \ipt -> do
          gain_ 2.0
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
            [ DA.xtypeRange
            , DA.min_ "0"
            , DA.max_ "100"
            , DA.step_ "1"
            , DA.value_ "50"
            , DL.input_
                ( traverse_
                    (valueAsNumber >=> setSlider)
                    <<< (=<<) fromEventTarget
                    <<< target
                )
            ]
            []
        ]
    , D.button
        [ DL.runOn DL.click $ start $> do
            ctx <- context
            ivl <- interval ctx
            myIvl <- liftST $ rant $ Tuple <$> random <*>
              ( dredge ivl.fevent
                  ( map (calcSlope 0.0 0.42 100.0 1.4)
                      slider
                  )
              )
            r <- run2 ctx (music myIvl.poll)
            setStop (r *> close ctx)
        , DL.runOn DL.click $ stop <#>
            (_ *> setStart unit)
        ]

        [ text $ oneOf
            [ start $> "Turn on"
            , stop $> "Turn off"
            ]
        ]
    ]
