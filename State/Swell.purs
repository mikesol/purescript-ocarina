module WAGS.Example.Docs.State.Swell where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Tuple.Nested ((/\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb)
import Deku.Control (text, plant)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Behavior (ABehavior, Behavior, behavior, sample, sampleBy, sample_, step, switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Time as Time
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (class IsEvent, bang, fix, fold, sampleOn, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Parameter (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2e)

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
swell :: Mouse -> Behavior Number
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

  fixB :: forall a. a -> (Behavior a -> Behavior a) -> Behavior a
  fixB a fn = behavior \s ->
    fix \event ->
      let
        b = fn (step a event)
      in
        { input: sample_ b s, output: sampleOn event s }

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
    -> ABehavior event t
    -> ABehavior event a
    -> ABehavior event a
  integral g initial t b =
    behavior \e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\) t x)
        z = fold approx y initial
      in
        sampleOn z e
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
    -> ABehavior event t
    -> ABehavior event t
    -> ABehavior event t
  integral' = integral (_ $ identity)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ StartStop) \push event -> do
      let
        startE = bang unit <|> event.start
        stopE = event.stop
      plant $ D.div_
        [ D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ startE $>
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
                , event.stop <#> (_ *> push.start unit)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , stopE $> "Turn off"
                ]
            ]
        ]
  )