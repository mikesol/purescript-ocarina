module WAGS.Example.Docs.Effects.FoldEx where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus)
import Data.Exists (mkExists)
import Data.Foldable (oneOf)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, match)
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (Event, filterMap, fold, mapAccum, sampleOn)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (class IsEvent, bang, biSampleOn)
import FRP.Event.Memoize (memoize)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain, periodicOsc)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent(..), SingleSubgraphPusher)
import WAGS.Example.Docs.Util (scrollToTop)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2)
import WAGS.Variant (injs_, prjs_)

px = Proxy :: Proxy """<section>
  <h2>Fold</h2>

  <p>The type of <code>fold</code> is:</p>

  <pre><code>fold
    :: forall event a b
    . IsEvent event
    => (a -> b -> b)
    -> event a
    -> b
    -> event b</code></pre>

  <p>Fold starts with some initial state <code>b</code> and, based on incoming events, allows you to change the state.</p>

  <p>One way <code>fold</code> is useful is to retain when certain actions happen. In the following example, we use <code>requestAnimationFrame</code> to animate the audio and we use four <code>fold</code>-s to store the ambitus and velocity of both vibrato and tremolo.</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p><code>fold</code> is so powerful because it allows us to localize state to <i>any</i> event. In the example above, instead of having a global state, our two folds allow for two <i>ad hoc</i> local states.</p>

</section>"""


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

foldEx :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent   -> Element Event payload
foldEx ccb dpage _ ev = px ~~
  { txt: nut $ text_ """module Main where

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
    ]"""
  , empl: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push event -> do -- here
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
                  ( (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map (/\) ss)) <#>
                      \(e /\ c) -> D.OnClick := cb
                        ( const $ e # match
                            { stop: \u -> u *> push start *> ccb (pure unit)
                            , start: \_ -> do
                                c
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
                                r' <- run2 ctx $ gain 0.0
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
                                let r = r' *> close ctx
                                ccb (r *> push start) -- here
                                push (stop r)
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
      )
  }