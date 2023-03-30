module Ocarina.Example.Nested where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event.VBus (V)
import Ocarina.Control (constant, gain, gain_, sinOsc)
import Ocarina.Core (Audible, bangOn, c1)
import Ocarina.Interpret (close, context)
import Ocarina.Properties as P
import Ocarina.Run (run2)

main :: Effect Unit
main = runInBody Deku.do
  setStart /\ start <- useState'
  setStop /\ stop <- useState'
  let
    startE = pure unit <|> start
    stopE = stop

    music :: Array (Audible _ _)
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
                    (_ *> setStart unit)
                , startE $> do
                    ctx <- context
                    r <- run2 ctx music
                    setStop (r *> close ctx)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , stopE $> "Turn off"
                ]
            ]
        ]
    ]
