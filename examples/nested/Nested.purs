module Ocarina.Example.Nested where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)

import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Control (constant, gain, gain_, sinOsc)
import Ocarina.Core (Audible, bangOn, c1)
import Ocarina.Interpret (close, context)
import Ocarina.Properties as P
import Ocarina.Run (run2)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V (startStop :: StartStop)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \push event -> do
      let
        startE = pure unit <|> event.startStop.start
        stopE = event.startStop.stop

        music :: forall lock. Array (Audible _ lock _)
        music =
          [ gain 0.0
              ( pure
                  ( P.gain
                      ( c1
                          ( gain_ 0.1
                              [ sinOsc 1.0
                                  ( bangOn <|> pure
                                      ( P.frequency
                                          ( c1
                                              ( gain_ 1.0
                                                  [ constant 5.0 bangOn
                                                  , gain_ 3.0 [ sinOsc 1.0 bangOn ]
                                                  ]
                                              )
                                          )
                                      )
                                  )
                              ]
                          )
                      )
                  )
              )
              [sinOsc 440.0 bangOn]

          ]
      D.div_
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