module Ocarina.Example.Docs.FixEx where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState')
import Deku.Pursx (pursx)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Event (Event)
import FRP.Event.AnimationFrame (animationFrame')
import FRP.Event.Class (fix, fold, sampleOnRight, withLast)
import FRP.Event.Mouse (down, getMouse, withButtons)
import FRP.Event.Time (withTime)
import FRP.Poll (Poll, poll, rant, sample, sampleBy, sample_, sham, step, switcher)
import Ocarina.Clock (withACTime)
import Ocarina.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..), SingleSubgraphPusher)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Properties as P
import Ocarina.Run (run2)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)

-- `swell` is an interactive function of time defined by a differential equation:
--
-- d^2s/dt^2
--   | mouse down = ⍺ - βs
--   | mouse up   = ɣ - δs - ε ds/dt
--
-- So the function exhibits either decay or growth depending on if
-- the mouse is pressed or not.
--
-- We can solve the differential equation by integration using `solve2'`.
swell :: Poll Instant -> Poll (Set.Set Int) -> Event Unit -> Poll Number
swell timeInSeconds' currentButtons down =
  fixB 2.0 \b ->
    integral' 2.0 timeInSeconds
      let
        db = fixB 10.0 \db_ ->
          integral' 10.0 timeInSeconds (f <$> currentButtons <*> b <*> db_)
      in
        switcher db (down $> db)
  where
  timeInSeconds = map (unInstant >>> unwrap) timeInSeconds'
  f bs s ds
    | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
    | otherwise = 2.0 * (4.0 - s)

  fixB :: forall a. a -> (Poll a -> Poll a) -> Poll a
  fixB a fn = poll \s ->
    sampleOnRight
      ( fix \event ->
          let
            b = fn (step a event)
          in
            sample_ b s
      )
      s


  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function approximates the integral using the trapezium rule at the
  -- | implicit sampling interval.
  -- |
  -- | The `Semiring` `a` should be a vector field over the field `t`. To represent
  -- | this, the user should provide a _grate_ which lifts a multiplication
  -- | function on `t` to a function on `a`. Simple examples where `t ~ a` can use
  -- | the `integral'` function instead.
  integral
    :: forall a t
     . Field t
    => Semiring a
    => (((a -> t) -> t) -> a)
    -> a
    -> Poll t
    -> Poll a
    -> Poll a
  integral g initial t b =
    poll \e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\) t x)
        z = fold (flip approx) initial y
      in
        sampleOnRight z e
    where
    approx { last: Nothing } s = s
    approx { now: t1 /\ a1, last: Just (t0 /\ a0) } s = s + g (\z -> z (a0 + a1) * (t1 - t0) / two)

    two :: t
    two = one + one

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function is a simpler version of `integral` where the function being
  -- | integrated takes values in the same field used to represent time.
  integral'
    :: forall t
     . Field t
    => t
    -> Poll t
    -> Poll t
    -> Poll t
  integral' = integral (_ $ identity)

fixEx :: CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Poll SingleSubgraphEvent -> Nut
fixEx ccb _ _ ev = pursx @Px
  { txt: text_
      """module Main

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Tuple.Nested ((/\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Poll (APoll, Poll, poll, sample, sampleBy, sample_, step, switcher)
import FRP.Poll.Mouse (buttons)
import FRP.Poll.Time as Time
import FRP.Event (memoize)

import FRP.Event.Class (class IsEvent, fix, fold, sampleOnRight, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import Ocarina.Clock(withACTime)
import Ocarina.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Core (AudioNumeric(..), _linear, bangOn)
import Ocarina.Properties as P
import Ocarina.Run (run2e)

type StartStop = V (start :: Unit, stop :: Effect Unit)

-- `swell` is an interactive function of time defined by a differential equation:
--
-- d^2s/dt^2
--   | mouse down = ⍺ - βs
--   | mouse up   = ɣ - δs - ε ds/dt
--
-- So the function exhibits either decay or growth depending on if
-- the mouse is pressed or not.
--
-- We can solve the differential equation by integration using `solve2'`.
swell :: Mouse -> Poll Number
swell mouse =
  fixB 2.0 \b ->
    integral' 2.0 (unwrap <$> Time.seconds)
      let
        db = fixB 10.0 \db_ ->
          integral' 10.0 (unwrap <$> Time.seconds) (f <$> buttons mouse <*> b <*> db_)
      in
        switcher db (down $> db)
  where
  f bs s ds
    | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
    | otherwise = 2.0 * (4.0 - s)

  fixB :: forall a. a -> (Poll a -> Poll a) -> Poll a
  fixB a fn = poll \s ->
    fix \event ->
      let
        b = fn (step a event)
      in
        { input: sample_ b s, output: sampleOnRight event s }

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function approximates the integral using the trapezium rule at the
  -- | implicit sampling interval.
  -- |
  -- | The `Semiring` `a` should be a vector field over the field `t`. To represent
  -- | this, the user should provide a _grate_ which lifts a multiplication
  -- | function on `t` to a function on `a`. Simple examples where `t ~ a` can use
  -- | the `integral'` function instead.
  integral
    :: forall event a t
     . IsEvent event
    => Field t
    => Semiring a
    => (((a -> t) -> t) -> a)
    -> a
    -> Poll t
    -> Poll a
    -> Poll a
  integral g initial t b =
    poll \e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\) t x)
        z = fold approx y initial
      in
        sampleOnRight z e
    where
    approx { last: Nothing } s = s
    approx { now: t1 /\ a1, last: Just (t0 /\ a0) } s = s + g (\z -> z (a0 + a1) * (t1 - t0) / two)

    two :: t
    two = one + one

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function is a simpler version of `integral` where the function being
  -- | integrated takes values in the same field used to represent time.
  integral'
    :: forall event t
     . IsEvent event
    => Field t
    => t
    -> Poll t
    -> Poll t
    -> Poll t
  integral' = integral (_ $ identity)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ StartStop) \push event -> do
      let
        startE = pure unit <|> event.start
        stopE = event.stop
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                event.stop <#> (_ *> push.start unit)
                startE $>
                  do
                    ctx <- context
                    c0h <- constant0Hack ctx
                    mouse <- getMouse
                    ri <- randomInt 0 10000
                    let
                      ttap (o /\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }
                      fund = 90.4
                      spcn = map (_ - 0.5) arbitrary
                      spc' = do
                        a <- spcn
                        b <- spcn
                        c <- spcn
                        d <- spcn
                        pure (a +> b +> c +> d +> V.empty)
                      spc = (/\) <$> spc' <*> spc'
                      spcs = { s0: _, s1: _, s2: _, s3: _ } <$> spc <*> spc <*> spc <*> spc
                      allSpcs = evalGen spcs { newSeed: mkSeed ri, size: 5 }
                    r <- run2e ctx
                      ( memoize
                          ( map (\{ acTime, value } -> acTime /\ value)
                              $ withACTime ctx
                              $ sample_ (swell mouse) animationFrameEvent
                          )
                          \swm ->
                            [ gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\x -> max (-0.4) $ 0.5 * (x - 1.0)) <$> swm
                                )
                                [ lowpass_ { frequency: fund, q: 20.0 }
                                    [ squareOsc_ fund ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\x -> max (-0.2) $ 0.4 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 4.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: (fund * 3.02)
                                        , spec: allSpcs.s0
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\x -> fund * 3.02 + 14.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\x -> max (-0.1) $ 0.2 * (x - 6.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 6.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 5.07
                                        , spec: allSpcs.s1
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\x -> fund * 5.07 + 18.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\x -> max 0.0 $ 0.2 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 8.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 7.13
                                        , spec: allSpcs.s2
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\x -> fund * 7.13 + 32.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\x -> max 0.0 $ 0.1 * (x - 7.0)) <$> swm
                                )
                                [ periodicOsc
                                    { frequency: fund * 9.14
                                    , spec: allSpcs.s3
                                    }
                                    ( bangOn <|>
                                        ( P.frequency
                                            <<< ttap
                                            <<< second (\x -> fund * 9.14 + 31.0 * (x - 1.0)) <$> swm
                                        )
                                    )
                                ]
                            ]
                      )
                    push.stop (r *> c0h *> close ctx)
            )
            [ text $ OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        ]
  )"""
  , empl:Deku.do
          setStart /\ start <- useState'
          setStop /\ stop <- useState'
          let
            startE = pure unit <|> start
            stopE = stop
          D.div_
            [ D.button
                    [ DL.runOn DL.click $ ((startE $> identity) <*> (pure (pure unit) <|> (map (\(SetCancel x) -> x) ev))) <#> \cncl ->
                        do
                          cncl
                          ctx <- context
                          c0h <- constant0Hack ctx
                          mouse <- getMouse
                          ri <- randomInt 0 10000
                          let
                            ttap (o /\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }
                            fund = 90.4
                            spcn = map (_ - 0.5) arbitrary
                            spc' = do
                              a <- spcn
                              b <- spcn
                              c <- spcn
                              d <- spcn
                              pure (a +> b +> c +> d +> V.empty)
                            spc = (/\) <$> spc' <*> spc'
                            spcs = { s0: _, s1: _, s2: _, s3: _ } <$> spc <*> spc <*> spc <*> spc
                            allSpcs = evalGen spcs { newSeed: mkSeed ri, size: 5 }
                          afe <- animationFrame'
                            (withButtons mouse >>> withTime >>> withACTime ctx)
                          dn <- down
                          swm <- liftST $ rant
                            ( Tuple <$> (sham $ map _.value.value.acTime afe.event) <*>
                                (swell (sham $ map _.value.time afe.event) (sham $ map _.buttons afe.event) (dn.event $> unit))
                            )
                          r' <- run2 ctx 
                                  [ gain 0.0
                                      ( P.gain
                                          <<< ttap
                                          <<< second (\x -> max (-0.4) $ 0.5 * (x - 1.0)) <$> swm.poll
                                      )
                                      [ lowpass_ { frequency: fund, q: 20.0 }
                                          [ squareOsc_ fund ]
                                      ]
                                  , gain 0.0
                                      ( P.gain
                                          <<< ttap
                                          <<< second (\x -> max (-0.2) $ 0.4 * (x - 3.0)) <$> swm.poll
                                      )
                                      [ bandpass_ { frequency: fund * 4.0, q: 20.0 }
                                          [ periodicOsc
                                              { frequency: (fund * 3.02)
                                              , spec: allSpcs.s0
                                              }
                                              ( bangOn <|>
                                                  ( P.frequency
                                                      <<< ttap
                                                      <<< second (\x -> fund * 3.02 + 14.0 * (x - 1.0)) <$> swm.poll
                                                  )
                                              )
                                          ]
                                      ]
                                  , gain 0.0
                                      ( P.gain
                                          <<< ttap
                                          <<< second (\x -> max (-0.1) $ 0.2 * (x - 6.0)) <$> swm.poll
                                      )
                                      [ bandpass_ { frequency: fund * 6.0, q: 20.0 }
                                          [ periodicOsc
                                              { frequency: fund * 5.07
                                              , spec: allSpcs.s1
                                              }
                                              ( bangOn <|>
                                                  ( P.frequency
                                                      <<< ttap
                                                      <<< second (\x -> fund * 5.07 + 18.0 * (x - 1.0)) <$> swm.poll
                                                  )
                                              )
                                          ]
                                      ]
                                  , gain 0.0
                                      ( P.gain
                                          <<< ttap
                                          <<< second (\x -> max 0.0 $ 0.2 * (x - 3.0)) <$> swm.poll
                                      )
                                      [ bandpass_ { frequency: fund * 8.0, q: 20.0 }
                                          [ periodicOsc
                                              { frequency: fund * 7.13
                                              , spec: allSpcs.s2
                                              }
                                              ( bangOn <|>
                                                  ( P.frequency
                                                      <<< ttap
                                                      <<< second (\x -> fund * 7.13 + 32.0 * (x - 1.0)) <$> swm.poll
                                                  )
                                              )
                                          ]
                                      ]
                                  , gain 0.0
                                      ( P.gain
                                          <<< ttap
                                          <<< second (\x -> max 0.0 $ 0.1 * (x - 7.0)) <$> swm.poll
                                      )
                                      [ periodicOsc
                                          { frequency: fund * 9.14
                                          , spec: allSpcs.s3
                                          }
                                          ( bangOn <|>
                                              ( P.frequency
                                                  <<< ttap
                                                  <<< second (\x -> fund * 9.14 + 31.0 * (x - 1.0)) <$> swm.poll
                                              )
                                          )
                                      ]
                                  ]
                          let r = r' *> c0h *> close ctx
                          ccb (r *> setStart unit) -- here
                          setStop r
                    , DL.runOn DL.click $ stop <#> (_ *> (ccb (pure unit) *> setStart unit))
                    ]
                
                [ text $ oneOf
                    [ startE $> "Turn on"
                    , stopE $> "Turn off"
                    ]
                ]
            ]
      
  }

type Px =  """<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in ocarina that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat polls that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-polls</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>"""
