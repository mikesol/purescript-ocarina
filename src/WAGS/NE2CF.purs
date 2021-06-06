module WAGS.NE2CF where

import Prelude
import Control.Comonad.Cofree (Cofree, hoistCofree, (:<))
import Control.Semigroupoid (composeFlipped)
import Data.Lens (_2, over)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested ((/\), type (/\))
import WAGS.Graph.Parameter (AudioParameterTransition(..), AudioParameter, AudioParameter_(..))
import WAGS.Math (calcSlope)

type TimeHeadroom
  = { time :: Number, headroom :: Number }

type ASDR
  = TimeHeadroom -> Cofree ((->) TimeHeadroom) (AudioParameter)

-- | From a non-empty list of times and values, make a cofree comonad that emits audio parameters
-- | Based on a current time and a look-ahead
makePiecewise :: NonEmpty List (Number /\ Number) -> ASDR
makePiecewise (a /\ b :| Nil) _ =
  AudioParameter
    { param: Just b
    , timeOffset: 0.0
    , transition: LinearRamp
    }
    :< makePiecewise (a /\ b :| Nil)

makePiecewise v@(a /\ b :| (Cons (c /\ d) e)) { time, headroom }
  | time <= c =
    let
      lookahead = time + headroom
    in
      ( if lookahead >= c then
          AudioParameter
            { param: Just d
            , timeOffset: c - time
            , transition: LinearRamp
            }
        else
          AudioParameter { param: Just (calcSlope a b c d time), timeOffset: 0.0, transition: LinearRamp }
      )
        :< makePiecewise v
  | otherwise = makePiecewise (c /\ d :| e) { time, headroom }

type NonEmptyToCofree a b
  = { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b

type NonEmptyToCofree' a
  = Number -> Cofree ((->) Number) a

nonEmptyToCofree' :: forall a. Maybe a -> NonEmpty List ((Number -> Boolean) /\ a) -> Number -> Cofree ((->) Number) a
nonEmptyToCofree' a b c =
  hoistCofree (\ftu n -> ftu { time: n, value: unit })
    (nonEmptyToCofree (map pure a) (map (over _2 pure) b) { time: c, value: unit })

nonEmptyToCofree :: forall a b. Maybe (a -> b) -> NonEmpty List ((Number -> Boolean) /\ (a -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
nonEmptyToCofree a b =
  nonEmptyToCofreeFull
    (map (composeFlipped _.value) a)
    (map (over _2 (composeFlipped _.value)) b)

nonEmptyToCofreeFull :: forall a b. Maybe ({ time :: Number, value :: a } -> b) -> NonEmpty List ((Number -> Boolean) /\ ({ time :: Number, value :: a } -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
nonEmptyToCofreeFull maybeOtherwise (h :| t) = go (h : t)
  where
  go :: List ((Number -> Boolean) /\ ({ time :: Number, value :: a } -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
  go Nil = case maybeOtherwise of
    Just f -> let q i = f i :< q in q
    Nothing -> go (h : t)

  go ((tf /\ vf) : b) = let q i@{ time } = if tf time then (vf i :< q) else go b i in q
