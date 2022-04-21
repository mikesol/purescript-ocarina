module WAGS.Example.Docs.Events.WAC where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Toplevel ((🚀))
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (behavior, sampleBy)
import FRP.Event (Event, keepLatest, makeEvent, subscribe)
import FRP.Event.Class (bang, filterMap)
import FRP.Event.Memoize (memoize)
import Type.Proxy (Proxy(..))
import WAGS.Clock (interval)
import WAGS.Control (bandpass_, gain, gain_, highpass_, sinOsc, triangleOsc)
import WAGS.Core (Node, SinOsc(..), fan, input)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioEnvelope(..), AudioOnOff(..), _off, _on, bangOn)
import WAGS.Properties (frequency, onOff)
import WAGS.Properties as P
import WAGS.Run (run2)
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
main = uii.init unit 🚀 \push event -> do
  let
    ss = bang (ssi.start unit) <|> filterMap uip.startStop event
    music :: forall lock. _ -> Array (Node _ lock _ _)
    music time = do
      let
        e0 = P.gain <<< AudioEnvelope
          <<<
            { p: map (mul 0.05) [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
            , d: 0.15
            , o: _
            }
          <<< add 0.03
        (oon :: Number -> Event SinOsc) =
           bang <<< onOff <<< AudioOnOff <<< { x: _on, o: _ }
        (oof :: Number -> Event SinOsc) = bang <<< onOff <<< AudioOnOff <<< { x: _off, o: _ } <<< add 0.22
      [ gain_ 1.0
          (map (\i -> gain 0.0 (map e0 time) [ sinOsc (toNumber i * 80.0 + 440.0)
            (keepLatest $ map (oneOf <$> sequence [ oon, oof ]) time) ]) (0 .. 29))
      ]
  D.div_
    [ D.div_
        [ D.button
            ( ss <#>
                \e -> D.OnClick := cb
                  ( const $ e # match
                      { stop: \u -> u *>
                          push start
                      , start: \_ -> do
                          ctx <- context
                          myIvl <- memoize
                            $ interval ctx 0.25
                            $ bang 0.25
                          r <- run2 ctx (music myIvl)
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
    ]