module WAGS.Graph.Parameter where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | A control-rate audio parameter as a newtype.
newtype AudioParameter
  = AudioParameter AudioParameter'

derive instance newtypeAudioParameter :: Newtype AudioParameter _

derive newtype instance eqAudioParameter :: Eq AudioParameter

derive newtype instance showAudioParameter :: Show AudioParameter

uop :: (Number -> Number) -> AudioParameter -> AudioParameter
uop f (AudioParameter a0@{ param: param0, timeOffset: timeOffset0, transition: transition0 }) = AudioParameter { param: f param0, timeOffset: timeOffset0, transition: transition0 }

bop :: (Number -> Number -> Number) -> AudioParameter -> AudioParameter -> AudioParameter
bop f (AudioParameter a0@{ param: param0, timeOffset: timeOffset0, transition: transition0 }) (AudioParameter a1@{ param: param1, timeOffset: timeOffset1, transition: transition1 }) = AudioParameter { param: f param0 param1, timeOffset: timeOffset0 + timeOffset1 / 2.0, transition: transition0 <> transition1 }

instance semiringAudioParameter :: Semiring AudioParameter where
  zero = param zero
  one = param one
  add = bop add
  mul = bop mul

instance ringAudioParameter :: Ring AudioParameter where
  sub = bop sub

instance divisionRingAudioParameter :: DivisionRing AudioParameter where
  recip = uop recip

instance commutativeRingAudioParameter :: CommutativeRing AudioParameter

instance euclideanRingAudioParameter :: EuclideanRing AudioParameter where
  degree (AudioParameter { param: param0 }) = degree param0
  div = bop div
  mod = bop mod

-- | A control-rate audio parameter.
-- |
-- | `param`: The parameter as a floating-point value.
-- | `timeOffset`: How far ahead of the current playhead to set the parameter. This can be used in conjunction with the `headroom` parameter in `run` to execute precisely-timed events. For example, if the `headroom` is `20ms` and an attack should happen in `10ms`, use `timeOffset: 10.0` to make sure that the taret parameter happens exactly at the point of attack.
-- | `transition`: Transition between two points in time.
type AudioParameter'
  = { param :: Number
    , timeOffset :: Number
    , transition :: AudioParameterTransition
    }

-- | A transition between two points in time.
-- | - `NoRamp` is a discrete step.
-- | - `LinearRamp` is linear interpolation between two values.
-- | - `ExponentialRamp` is exponential interpolation between two values.
-- | - `Immediately` erases the current value and replaces it with this one. Different than `NoRamp` in that it does not take into account scheduling and executes immediately. Useful for responsive instruments.
data AudioParameterTransition
  = NoRamp
  | LinearRamp
  | ExponentialRamp
  | Immediately

derive instance eqAudioParameterTransition :: Eq AudioParameterTransition

derive instance genericAudioParameterTransition :: Generic AudioParameterTransition _

instance showAudioParameterTransition :: Show AudioParameterTransition where
  show = genericShow

instance semigroupAudioParameterTransition :: Semigroup AudioParameterTransition where
  append Immediately _ = Immediately
  append _ Immediately = Immediately
  append ExponentialRamp _ = ExponentialRamp
  append _ ExponentialRamp = ExponentialRamp
  append LinearRamp _ = LinearRamp
  append _ LinearRamp = LinearRamp
  append _ _ = NoRamp

-- | Create an audio parameter from a number using default parameters.
param :: Number -> AudioParameter
param =
  AudioParameter
    <<< defaultParam
        { param = _
        }

-- | A default audio parameter.
-- |
-- | defaultParam = { param: 0.0, timeOffset: 0.0, transition: LinearRamp }
defaultParam :: AudioParameter'
defaultParam = { param: 0.0, timeOffset: 0.0, transition: LinearRamp }
