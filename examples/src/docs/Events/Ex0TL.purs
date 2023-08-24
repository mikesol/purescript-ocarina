module Ocarina.Example.Docs.Events.Ex0TL where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Number (pow)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Ocarina.Control (gain_, gain, sinOsc)
import Ocarina.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = pure $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = pure $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = pure $ P.gain
  $ AudioEnvelope
      { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
      , d: 0.8
      , o
      }

-- a single cell with four oscillators,
-- each of which have the envelope applied
cell = lcmap toNumber \i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 `pow` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 `pow` i)))
    env' x = env (x + 0.3 * (i * (1.005 `pow` i)))
    strand x y =
      gain 0.0 (env' x) [ sinOsc (200.0 + i * y) (ooo' x) ]
  [ strand 0.2 4.0
  , strand 0.3 6.0
  , strand 0.45 14.0
  , strand 0.7 20.0
  ]

main :: Effect Unit
main = runInBody Deku.do
  push /\ event <- useState Init
  D.div_
    [ D.button
        [ DL.runOn DL.click $ event <#> case _ of
            Stop u -> u *> push Start
            _ -> do
              r <- run2_
                [ gain_ 1.0
                    -- we create 100 cells
                    $ join
                    $ cell <$> 0 .. 100
                ]
              push $ Stop r
        ]
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ]