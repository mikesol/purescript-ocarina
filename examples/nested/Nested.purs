module WAGS.Example.Nested where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Deku.Attribute (attr, cb)
import Deku.Control (text, plant)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event.Class (bang)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (constant, gain, gain_, sinOsc)
import WAGS.Core (Node, bangOn)
import WAGS.Core (Node, bangOn, c1)
import WAGS.Interpret (close, context)
import WAGS.Properties as P
import WAGS.Run (run2)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V (startStop :: StartStop)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \push event -> do
      let
        startE = bang unit <|> event.startStop.start
        stopE = event.startStop.stop

        music :: forall lock. Array (Node _ lock _)
        music =
          [ gain 0.0
              ( bang
                  (P.gain (c1 (gain_ 0.1 (sinOsc 1.0 (bangOn <|> bang (P.frequency (c1 (gain_ 1.0 [ constant 5.0 bangOn, gain_ 3.0 (sinOsc 1.0 bangOn) ]))))))))
              )
              (sinOsc 440.0 bangOn)

          ]
      plant $ D.div_
        [ D.div_
            [ D.button
                ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                    [ stopE <#>
                        (_ *> push.startStop.start unit)
                    , startE $> do
                        ctx <- context
                        r <- run2 ctx music
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