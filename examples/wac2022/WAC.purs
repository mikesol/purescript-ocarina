module WAGS.Example.WAC where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Foldable (oneOf)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Data.Variant (Variant, match)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody, runInBody1)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, bus, keepLatest, memoize, subscribe)
import FRP.Event.Class (bang, filterMap)
import Type.Proxy (Proxy(..))
import WAGS.Clock (interval)
import WAGS.Control (gain, gain_, sinOsc)
import WAGS.Core (Node, SinOsc)
import WAGS.Interpret (close, context)
import WAGS.Parameter (AudioEnvelope(..), AudioOnOff(..), _off, _on)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2, run2e)
import WAGS.Variant (injs_, prjs_)

type StartStop = Variant (start :: Unit, stop :: Effect Unit)
ssi = injs_ (Proxy :: _ StartStop)
start = uii.startStop (ssi.start unit)
stop r = uii.startStop (ssi.stop r)

type UIEvents = Variant
  ( init :: Unit
  , startStop :: StartStop
  )

uii = injs_ (Proxy :: _ UIEvents)
uip = prjs_ (Proxy :: _ UIEvents)

main :: Effect Unit
main = runInBody1
  ( bus \push event -> do
      let
        ss = bang (ssi.start unit) <|> filterMap uip.startStop event
        music :: forall lock. _ -> Array (Node _ lock _)
        music time = do
          let
            adsr = AudioEnvelope
              <<<
                { p: map (mul 0.05) [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                , d: 0.15
                , o: _
                }
            e0 = P.gain <<< adsr <<< add 0.03
            oon =
              bang <<< onOff <<< AudioOnOff <<< { x: _on, o: _ }
            oof = bang <<< onOff <<< AudioOnOff <<< { x: _off, o: _ } <<< add 0.22
          [ gain_ 1.0
              ( map
                  ( \i -> gain 0.0 (map e0 time)
                      [ sinOsc (toNumber i * 80.0 + 440.0)
                          (keepLatest $ map (oneOf <$> sequence [ oon, oof ]) time)
                      ]
                  )
                  (0 .. 29)
              )
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
                              r <- run2e ctx
                                ( memoize
                                    (interval ctx 0.25 (bang 0.25))
                                    music
                                )
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
  )