module WAGS.Example.Docs.State.Fold where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, match)
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel ((🚀))
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang, filterMap, fold, mapAccum, sampleOn)
import FRP.Event.Memoize (memoize)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain, periodicOsc)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2)
import WAGS.Variant (injs_, prjs_)

type Cbx = Variant (cbx0 :: Unit, cbx1 :: Unit, cbx2 :: Unit, cbx3 :: Unit)
cbi = injs_ (Proxy :: _ Cbx)
cbp = prjs_ (Proxy :: _ Cbx)

type StartStop = Variant (start :: Unit, stop :: Effect Unit)
ssi = injs_ (Proxy :: _ StartStop)
start = uii.startStop (ssi.start unit)
stop r = uii.startStop (ssi.stop r)

type UIEvents = Variant
  ( init :: Unit
  , startStop :: StartStop
  , cbx :: Cbx
  )

uii = injs_ (Proxy :: _ UIEvents)
uip = prjs_ (Proxy :: _ UIEvents)

main :: Effect Unit
main = start 🚀 \push event -> do
  let
    ss = bang (ssi.start unit) <|> filterMap uip.startStop event
    cbx = filterMap uip.cbx event
    chkState e = step false $ fold (const not) (filterMap e cbx) false
    cbx0 = chkState cbp.cbx0
    cbx1 = chkState cbp.cbx1
    cbx2 = chkState cbp.cbx2
    cbx3 = chkState cbp.cbx3
  D.div_
    [ D.button
        ( ss <#>
            \e -> D.OnClick := cb
              ( const $ e # match
                  { stop: \u -> u *> push start
                  , start: \_ -> do
                      ctx <- context
                      afe <- animationFrameEvent
                      acTime <- memoize
                         $ map (add 0.04 <<< _.acTime)
                         $ withACTime ctx afe
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
                        ev0 = cevt 8.0 cbx0 acTime
                        ev1 = map (if _ then 4.0 else 1.0) $ sample_ cbx1 acTime
                        ev2 = cevt 4.0 cbx2 acTime
                        ev3 = map (if _ then 4.0 else 1.0) $ sample_ cbx3 acTime
                        evs f a = sampleOn acTime
                          $ map ($)
                          $ sampleOn a
                          $ { f: _, a: _, t: _ } <$> f
                      r <- run2 ctx $ gain 0.0
                        ( evs ev0 ev1 <#> \{ f, a, t } -> P.gain $ AudioNumeric
                            { n: calcSlope 1.0 0.01 4.0 0.15 a * sin (pi * f) + 0.15
                            , o: t
                            , t: _linear
                            }
                        )
                        ( periodicOsc
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
                        )
                      push $ (stop (r *> close ctx))
                  }
              )
        )
        [ text $ ss <#> match
            { stop: \_ -> "Turn off"
            , start: \_ -> "Turn on"
            }
        ]
    , D.div
        ( ss <#> match
            { stop: \_ -> D.Style := "display:block;"
            , start: \_ -> D.Style := "display:none;"
            }
        )
        ( map
            ( \e -> D.input
                ( oneOf
                    [ bang (D.Xtype := "checkbox")
                    , bang (D.OnClick := cb (const $ push (uii.cbx $ e unit)))
                    , (const $ D.Checked := "false") <$> ss
                    ]
                )
                []
            )
            [ cbi.cbx0, cbi.cbx1, cbi.cbx2, cbi.cbx3 ]
        )
    ]