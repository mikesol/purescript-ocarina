module Ocarina.Example.Docs.State.Swell where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Control (text)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState')
import Deku.Toplevel (runInBody)
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
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.Properties as P
import Ocarina.Run (run2)
import QualifiedDo.Alt as OneOf
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

main :: Effect Unit
main = runInBody Deku.do
  setStart /\ start <- useState'
  setStop /\ stop <- useState'
  let
    startE = pure unit <|> start
    stopE = stop
  D.div_
    [ D.button
        [ DL.runOn DL.click $ stop <#> (_ *> setStart unit)
        , DL.runOn DL.click $ startE $>
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
              afe <- animationFrame'
                (withButtons mouse >>> withTime >>> withACTime ctx)
              dn <- down
              swm <- liftST $ rant
                ( Tuple <$> (sham $ map _.value.value.acTime afe.event) <*>
                    (swell (sham $ map _.value.time afe.event) (sham $ map _.buttons afe.event) (dn.event $> unit))
                )
              r <- run2 ctx
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

              setStop (r *> c0h *> afe.unsubscribe *> liftST swm.unsubscribe *> dn.unsubscribe *> close ctx)
        ]
        [ text $ OneOf.do
            startE $> "Turn on"
            stopE $> "Turn off"
        ]
    ]