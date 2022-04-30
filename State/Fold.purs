module WAGS.Example.Docs.State.Fold where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Tuple.Nested ((/\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, plant, text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang, fold, mapAccum, sampleOn)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain, periodicOsc)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2e)

type Cbx = V (cbx0 :: Unit, cbx1 :: Unit, cbx2 :: Unit, cbx3 :: Unit)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , cbx :: Cbx
  )

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \push event -> do
      let
        startE = bang unit <|> event.startStop.start
        stopE = event.startStop.stop
        chkState e = step false $ fold (const not) e false
        cbx0 = chkState event.cbx.cbx0
        cbx1 = chkState event.cbx.cbx1
        cbx2 = chkState event.cbx.cbx2
        cbx3 = chkState event.cbx.cbx3
      plant $ D.div_
        [ D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ startE $> do
                    ctx <- context
                    c0h <- constant0Hack ctx
                    let
                      cevt fast b tm = mapAccum
                        ( \(oo /\ act) (pact /\ pt) ->
                            let
                              tn = pt +
                                ( (act - pact) *
                                    (if oo then fast else 1.0)
                                )
                            in
                              ((act /\ tn) /\ tn)
                        )
                        (sampleBy (/\) b tm)
                        (0.0 /\ 0.0)

                    r <- run2e ctx
                      ( memoize
                          ( map (add 0.04 <<< _.acTime)
                              $ withACTime ctx animationFrameEvent
                          )
                          \acTime ->
                            let
                              ev0 = cevt 8.0 cbx0 acTime
                              ev1 = map (if _ then 4.0 else 1.0) $ sample_ cbx1 acTime
                              ev2 = cevt 4.0 cbx2 acTime
                              ev3 = map (if _ then 4.0 else 1.0) $ sample_ cbx3 acTime
                              evs f a = sampleOn acTime
                                $ map ($)
                                $ sampleOn a
                                $ { f: _, a: _, t: _ } <$> f
                            in
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
                                      ( oneOf
                                          [ bangOn
                                          , evs ev2 ev3 <#> \{ f, a, t } -> P.frequency
                                              $ AudioNumeric
                                                { n: 325.6 +
                                                    (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                                , o: t
                                                , t: _linear
                                                }
                                          ]
                                      )
                                  ]
                              ]
                      )
                    push.startStop.stop (r *> c0h *> close ctx)
                , stopE <#> (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , stopE $> "Turn off"
                ]
            ]
        , D.div
            ( oneOfMap (map (attr D.Style))
                [ stopE $> "display:block;"
                , startE $> "display:none;"
                ]
            )
            ( map
                ( \e -> D.input
                    ( oneOf
                        [ bang (D.Xtype := "checkbox")
                        , bang (D.OnClick := cb (const (e unit)))
                        , startE $> (D.Checked := "false")
                        ]
                    )
                    blank
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )