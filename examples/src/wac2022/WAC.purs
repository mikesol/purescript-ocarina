module Ocarina.Example.WAC where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Data.Array ((..))
import Data.Foldable (oneOf)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (create, keepLatest)
import FRP.Poll (rant, sham)
import Ocarina.Clock (interval)
import Ocarina.Control (gain, gain_, sinOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), AudioOnOff(AudioOnOff), _off, _on)
import Ocarina.Interpret (close, context)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2)

main :: Effect Unit
main = runInBody Deku.do
  setStart /\ start <- useState'
  setStop /\ stop <- useState'
  let
    startE = pure unit <|> start
    stopE = stop

    music :: _ -> Array (Audible _ _)
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
            [ DL.runOn DL.click $ stop <#>
                (_ *> setStart unit)
            , DL.runOn DL.click $ start $> do
                ctx <- context
                ep <- liftST create
                ii <- interval ctx ep.event
                rt <- liftST $ rant (sham ii.event <|> pure 0.25)
                r <- run2 ctx (music rt.poll)
                ep.push 0.25
                setStop (r *> ii.unsubscribe *> liftST rt.unsubscribe *> close ctx)
            ]
            [ text $ oneOf
                [ startE $> "Turn on"
                , stopE $> "Turn off"
                ]
            ]
        ]
    ]
