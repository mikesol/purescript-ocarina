module Ocarina.Example.WAC where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Foldable (oneOf, oneOfMap)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event (keepLatest, memoize)

import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (interval)
import Ocarina.Control (gain, gain_, sinOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), AudioOnOff(AudioOnOff), _off, _on)
import Ocarina.Interpret (close, context)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( init :: Unit
  , startStop :: StartStop
  )

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \push event -> do
      let
        startE = pure unit <|> event.startStop.start
        stopE = event.startStop.stop
        music :: forall lock. _ -> Array (Audible _ lock _)
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
              pure <<< onOff <<< AudioOnOff <<< { x: _on, o: _ }
            oof = pure <<< onOff <<< AudioOnOff <<< { x: _off, o: _ } <<< add 0.22
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
                ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                    [ event.startStop.stop <#>
                        (_ *> push.startStop.start unit)
                    , event.startStop.start $> do
                        ctx <- context
                        r <- run2e ctx
                          ( memoize
                              (interval ctx 0.25 (pure 0.25))
                              music
                          )
                        push.startStop.stop (r *> close ctx)
                    ]
                )
                [ text $ oneOf
                    [ startE $> "Turn on"
                    , stopE $> "Turn off"
                    ]
                ]
            ]
        ]
  )