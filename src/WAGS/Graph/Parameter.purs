module WAGS.Graph.Parameter where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | A control-rate audio parameter as a newtype.
newtype AudioParameter_ a
  = AudioParameter (AudioParameter_' a)

type AudioParameter
  = AudioParameter_ Number

derive instance newtypeAudioParameter :: Newtype (AudioParameter_ a) _

derive newtype instance eqAudioParameter :: Eq a => Eq (AudioParameter_ a)

derive newtype instance showAudioParameter :: Show a => Show (AudioParameter_ a)

uop :: forall a. (a -> a) -> AudioParameter_ a -> AudioParameter_ a
uop f (AudioParameter a0@{ param: param0, timeOffset: timeOffset0, transition: transition0, forceSet: forceSet0 }) = AudioParameter { param: f <$> param0, timeOffset: timeOffset0, transition: transition0, forceSet: forceSet0 }

bop :: forall a. (a -> a -> a) -> AudioParameter_ a -> AudioParameter_ a -> AudioParameter_ a
bop f (AudioParameter a0@{ param: param0, timeOffset: timeOffset0, transition: transition0, forceSet: forceSet0 }) (AudioParameter a1@{ param: param1, timeOffset: timeOffset1, transition: transition1, forceSet: forceSet1 }) = AudioParameter { param: f <$> param0 <*> param1, timeOffset: timeOffset0 + timeOffset1 / 2.0, transition: transition0 <> transition1, forceSet: forceSet0 || forceSet1 }

instance semiringAudioParameter :: Semiring a => Semiring (AudioParameter_ a) where
  zero = AudioParameter { param: Just zero, timeOffset: 0.0, transition: LinearRamp, forceSet: false }
  one = AudioParameter { param: Just one, timeOffset: 0.0, transition: LinearRamp, forceSet: false }
  add = bop add
  mul = bop mul

instance ringAudioParameter :: Ring a => Ring (AudioParameter_ a) where
  sub = bop sub

instance divisionRingAudioParameter :: DivisionRing a => DivisionRing (AudioParameter_ a) where
  recip = uop recip

instance commutativeRingAudioParameter :: CommutativeRing a => CommutativeRing (AudioParameter_ a)

instance euclideanRingAudioParameter :: EuclideanRing a => EuclideanRing (AudioParameter_ a) where
  degree (AudioParameter { param: param0 }) = maybe 0 degree param0
  div = bop div
  mod = bop mod

-- | A control-rate audio parameter.
-- |
-- | `param`: The parameter as a floating-point value _or_ an instruction to cancel all future parameters.
-- | `timeOffset`: How far ahead of the current playhead to set the parameter. This can be used in conjunction with the `headroom` parameter in `run` to execute precisely-timed events. For example, if the `headroom` is `20ms` and an attack should happen in `10ms`, use `timeOffset: 10.0` to make sure that the taret parameter happens exactly at the point of attack.
-- | `transition`: Transition between two points in time.
-- | `forceSet`: Should we force the setting of this parameter?
type AudioParameter_' a
  = { param :: Maybe a
    , timeOffset :: Number
    , transition :: AudioParameterTransition
    , forceSet :: Boolean
    }

type AudioParameter'
  = AudioParameter_' Number

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
    <<< Just

ff :: forall a. Number -> AudioParameter_ a -> AudioParameter_ a
ff n (AudioParameter i) = AudioParameter (i { timeOffset = i.timeOffset + n })

-- | A default audio parameter.
-- |
-- | defaultParam = { param: 0.0, timeOffset: 0.0, transition: LinearRamp }
defaultParam :: AudioParameter'
defaultParam = { param: Just 0.0, timeOffset: 0.0, transition: LinearRamp, forceSet: false }
