module Ocarina.Example.Docs.State.Fold where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Number (pi, sin)
import Data.Tuple.Nested ((/\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Control (text)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class (fold, mapAccum, sampleOnRight)
import FRP.Poll (rant, sampleBy, sample_, sham)
import Ocarina.Clock (withACTime)
import Ocarina.Control (gain, periodicOsc)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Math (calcSlope)
import Ocarina.Properties as P
import Ocarina.Run (run2)
import QualifiedDo.Alt as OneOf

main :: Effect Unit
main = runInBody Deku.do
  setCbx0 /\ cbx0' <- useState'
  setCbx1 /\ cbx1' <- useState'
  setCbx2 /\ cbx2' <- useState'
  setCbx3 /\ cbx3' <- useState'
  setStart /\ startE <- useState unit
  setStop /\ stopE <- useState'
  let
    chkState e = fold (\a _ -> not a) false e
    cbx0 = chkState cbx0'
    cbx1 = chkState cbx1'
    cbx2 = chkState cbx2'
    cbx3 = chkState cbx3'
  D.div_
    [ D.button
        [ DL.runOn DL.click $ stopE <#> (_ *> setStart unit)
        , DL.runOn DL.click $ startE $> do
            ctx <- context
            c0h <- constant0Hack ctx
            let
              cevt fast b tm = mapAccum
                ( \(pact /\ pt) (oo /\ act) ->
                    let
                      tn = pt +
                        ( (act - pact) *
                            (if oo then fast else 1.0)
                        )
                    in
                      ((act /\ tn) /\ tn)
                )
                (0.0 /\ 0.0)
                (sampleBy (/\) b tm)
            afe <- animationFrame
            acTime <- liftST $ rant
              ( sham
                  ( map (add 0.04 <<< _.acTime)
                      $ withACTime ctx afe.event
                  )
              )
            r <- run2 ctx do
              let
                ev0 = cevt 8.0 cbx0 acTime.poll
                ev1 = map (if _ then 4.0 else 1.0) $ sample_ cbx1 acTime.poll
                ev2 = cevt 4.0 cbx2 acTime.poll
                ev3 = map (if _ then 4.0 else 1.0) $ sample_ cbx3 acTime.poll
                evs f a = sampleOnRight acTime.poll
                  $ map ($)
                  $ sampleOnRight a
                  $ { f: _, a: _, t: _ } <$> f

              [ gain 0.0
                  ( evs ev0 ev1 <#> \{ f, a, t } -> P.gain $ AudioNumeric
                      { n: calcSlope 1.0 0.01 4.0 0.15 a * sin (pi * f) + 0.15
                      , o: t
                      , t: _linear
                      }
                  )
                  [ periodicOsc
                      { frequency: 325.6
                      , spec: (0.3 +> -0.1 +> 0.7 +> -0.4 +> V.empty)
                          /\ (0.6 +> 0.3 +> 0.2 +> 0.0 +> V.empty)
                      }
                      ( OneOf.do
                          bangOn
                          evs ev2 ev3 <#> \{ f, a, t } -> P.frequency
                            $ AudioNumeric
                                { n: 325.6 +
                                    (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                , o: t
                                , t: _linear
                                }
                      )
                  ]
              ]
            setStop (r *> c0h *> close ctx)
        ]
        [ text OneOf.do
            startE $> "Turn on"
            stopE $> "Turn off"
        ]
    , D.div
        [ DA.style $ stopE $> "display:block;"
        , DA.style $ startE $> "display:none;"
        ]
        ( map
            ( \e -> D.input
                [ DA.xtype_ "checkbox"
                , DL.click_ \_ -> e unit
                , DA.checked $ startE $> "false"
                ]
                []
            )
            [ setCbx0, setCbx1, setCbx2, setCbx3 ]
        )
    ]
