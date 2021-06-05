module WAGS.Graph.Parameter where

import Prelude hiding (apply)
import Data.Function (apply)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Record as R
import Type.Proxy (Proxy(..))

-- | A control-rate audio parameter as a newtype.
newtype AudioParameter_ a
  = AudioParameter (AudioParameter_' a)

type AudioParameter
  = AudioParameter_ Number

derive instance eqAudioParameter :: Eq a => Eq (AudioParameter_ a)

derive instance functorAudioParameter :: Functor AudioParameter_

instance applyAudioParameter :: Apply AudioParameter_ where
  apply = bop apply

instance applicativeAudioParameter :: Applicative AudioParameter_ where
  pure = param

instance bindAudioParameter :: Bind AudioParameter_ where
  bind ma@(AudioParameter i@{ param: Nothing }) _ = AudioParameter (i { param = Nothing })
  bind ma@(AudioParameter i@{ param: Just a }) f = AudioParameter (R.set (Proxy :: _ "param") (Just identity) i) <*> f a

instance monadAudioParameter :: Monad AudioParameter_

derive instance newtypeAudioParameter :: Newtype (AudioParameter_ a) _

derive newtype instance showAudioParameter :: Show a => Show (AudioParameter_ a)

uop :: forall a b. (a -> b) -> AudioParameter_ a -> AudioParameter_ b
uop f (AudioParameter a0@{ param: param0, timeOffset: timeOffset0, transition: transition0, forceSet: forceSet0 }) = AudioParameter { param: f <$> param0, timeOffset: timeOffset0, transition: transition0, forceSet: forceSet0 }

bop :: forall a b c. (a -> b -> c) -> AudioParameter_ a -> AudioParameter_ b -> AudioParameter_ c
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
param :: forall a. a -> AudioParameter_ a
param a = AudioParameter (R.set (Proxy :: _ "param") (Just a) defaultParam)

ff :: forall a. Number -> AudioParameter_ a -> AudioParameter_ a
ff n (AudioParameter i) = AudioParameter (i { timeOffset = i.timeOffset + n })

modTime :: forall a. (Number -> Number) -> AudioParameter_ a -> AudioParameter_ a
modTime f (AudioParameter i) = AudioParameter (i { timeOffset = f i.timeOffset })

modParam :: forall a. (a -> a) -> AudioParameter_ a -> AudioParameter_ a
modParam f (AudioParameter i) = AudioParameter (i { param = f <$> i.param })

forceSet :: forall a. AudioParameter_ a -> AudioParameter_ a
forceSet = modForceSet (const true)

unForceSet :: forall a. AudioParameter_ a -> AudioParameter_ a
unForceSet = modForceSet (const false)

modForceSet :: forall a. (Boolean -> Boolean) -> AudioParameter_ a -> AudioParameter_ a
modForceSet f (AudioParameter i) = AudioParameter (i { forceSet = f i.forceSet })

modRamp :: forall a. (AudioParameterTransition -> AudioParameterTransition) -> AudioParameter_ a -> AudioParameter_ a
modRamp f (AudioParameter i) = AudioParameter (i { transition = f i.transition })

noRamp :: forall a. AudioParameter_ a -> AudioParameter_ a
noRamp = modRamp (const NoRamp)

linearRamp :: forall a. AudioParameter_ a -> AudioParameter_ a
linearRamp = modRamp (const LinearRamp)

exponentialRamp :: forall a. AudioParameter_ a -> AudioParameter_ a
exponentialRamp = modRamp (const ExponentialRamp)

immediately :: forall a. AudioParameter_ a -> AudioParameter_ a
immediately = modRamp (const Immediately)

-- | A default audio parameter.
-- |
-- | defaultParam = { param: 0.0, timeOffset: 0.0, transition: LinearRamp }
defaultParam :: AudioParameter'
defaultParam = { param: Just 0.0, timeOffset: 0.0, transition: LinearRamp, forceSet: false }
