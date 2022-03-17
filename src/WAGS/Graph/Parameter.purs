module WAGS.Graph.Parameter where

import Prelude hiding (apply)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens, over, set, view)
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, inj, match)
import Data.Variant.Maybe (Maybe)
import Type.Proxy (Proxy(..))

newtype AudioParameter = AudioParameter
  ( Variant
      ( singleNumber :: AudioSingleNumber
      , cancellation :: AudioParameterCancellation
      , envelope :: AudioEnvelope
      )
  )

derive instance eqAudioParameter :: Eq AudioParameter
derive instance ordAudioParameter :: Ord AudioParameter
derive instance newtypeAudioParameter :: Newtype AudioParameter _
derive newtype instance showAudioParameter :: Show AudioParameter

newtype AudioParameterCancellation = AudioParameterCancellation
  { timeOffset :: Number
  , hold :: Boolean
  }

derive instance eqAudioParameterCancellation :: Eq AudioParameterCancellation
derive instance ordAudioParameterCancellation :: Ord AudioParameterCancellation
derive instance newtypeAudioParameterCancellation ::
  Newtype AudioParameterCancellation _

derive newtype instance showAudioParameterCancellation ::
  Show AudioParameterCancellation

newtype AudioSingleNumber = AudioSingleNumber
  { param :: Number
  , timeOffset :: Number
  , transition :: AudioSingleNumberTransition
  }

derive instance eqAudioSingleNumber :: Eq AudioSingleNumber
derive instance ordAudioSingleNumber :: Ord AudioSingleNumber
derive instance newtypeAudioSingleNumber :: Newtype AudioSingleNumber _
derive newtype instance showAudioSingleNumber :: Show AudioSingleNumber

instance semigroupAudioSingleNumber :: Semigroup AudioSingleNumber where
  append = bop (+)

instance monoidAudioSingleNumber :: Monoid AudioSingleNumber where
  mempty = AudioSingleNumber
    { param: zero
    , timeOffset: zero
    , transition: _immediately
    }

newtype AudioEnvelope = AudioEnvelope
  { values :: NonEmptyArray Number
  , timeOffset :: Number
  , duration :: Number
  }

derive instance eqAudioEnvelope :: Eq AudioEnvelope
derive instance ordAudioEnvelope :: Ord AudioEnvelope
derive instance newtypeAudioEnvelope :: Newtype AudioEnvelope _
derive newtype instance showAudioEnvelope :: Show AudioEnvelope

-- TODO: add semigroup, monoid, semiring etc for AudioEnvelope

-- | Term-level constructor for a generator being on or off
newtype OnOff = OnOff
  ( Variant
      ( on :: Unit
      , off :: Unit
      -- turns off immediately and then on, good for loops.
      -- todo: because of the way audioParameter works, this
      -- is forced to stop immediately
      -- this almost always is fine, but for more fine-grained control
      -- we'll need a different abstraction
      , offOn :: Unit
      )
  )

_on :: OnOff
_on = OnOff $ inj (Proxy :: _ "on") unit

_off :: OnOff
_off = OnOff $ inj (Proxy :: _ "off") unit

_offOn :: OnOff
_offOn = OnOff $ inj (Proxy :: _ "offOn") unit

derive instance newtypeOnOff :: Newtype OnOff _
derive instance eqOnOff :: Eq OnOff
derive instance ordOnOff :: Ord OnOff
derive instance genericOnOff :: Generic OnOff _

instance showOnOff :: Show OnOff where
  show = unwrap >>> match
    { on: const "on", off: const "off", offOn: const "offOn" }

newtype AudioOnOff = AudioOnOff
  { onOff :: OnOff
  , timeOffset :: Number
  }

derive instance eqAudioOnOff :: Eq AudioOnOff
derive instance ordAudioOnOff :: Ord AudioOnOff
derive instance newtypeAudioOnOff :: Newtype AudioOnOff _
derive newtype instance showAudioOnOff :: Show AudioOnOff

lensOnOff :: Lens' AudioOnOff OnOff
lensOnOff = lens (unwrap >>> _.onOff)
  (\(AudioOnOff s) -> AudioOnOff <<< s { onOff = _ })

lensParam :: Lens' AudioSingleNumber Number
lensParam = lens (unwrap >>> _.param)
  (\(AudioSingleNumber s) -> AudioSingleNumber <<< s { param = _ })

bop
  :: (Number -> Number -> Number)
  -> AudioSingleNumber
  -> AudioSingleNumber
  -> AudioSingleNumber
bop f (AudioSingleNumber xxx) (AudioSingleNumber yyy) = AudioSingleNumber
  { param: f xxx.param yyy.param
  , timeOffset: (xxx.timeOffset + yyy.timeOffset) / 2.0
  , transition: xxx.transition <> yyy.transition
  }

instance semiringAudioSingleNumber :: Semiring AudioSingleNumber where
  zero = AudioSingleNumber
    { param: zero
    , timeOffset: zero
    , transition: _linearRamp
    }
  one = AudioSingleNumber
    { param: one
    , timeOffset: zero
    , transition: _linearRamp
    }
  add = bop add
  mul = bop mul

instance ringAudioSingleNumber :: Ring AudioSingleNumber where
  sub = bop sub

instance divisionRingAudioSingleNumber :: DivisionRing AudioSingleNumber where
  recip = over lensParam recip

instance commutativeRingAudioSingleNumber :: CommutativeRing AudioSingleNumber

instance euclideanRingAudioSingleNumber :: EuclideanRing AudioSingleNumber where
  degree = degree <<< _.param <<< unwrap
  div = bop div
  mod = bop mod

-- | A transition between two points in time.
-- | - `_noRamp` is a discrete step.
-- | - `_linearRamp` is linear interpolation between two values.
-- | - `ExponentialRamp` is exponential interpolation between two values.
-- | - `Immediately` erases the current value and replaces it with this one. Different than `_noRamp` in that it does not take into account scheduling and executes immediately. Useful for responsive instruments.
newtype AudioSingleNumberTransition = AudioSingleNumberTransition
  ( Variant
      ( noRamp :: Unit
      , linearRamp :: Unit
      , exponentialRamp :: Unit
      , immediately :: Unit
      )
  )

_noRamp :: AudioSingleNumberTransition
_noRamp = AudioSingleNumberTransition $ inj (Proxy :: _ "noRamp") unit

_linearRamp :: AudioSingleNumberTransition
_linearRamp = AudioSingleNumberTransition $ inj (Proxy :: _ "linearRamp") unit

_exponentialRamp :: AudioSingleNumberTransition
_exponentialRamp = AudioSingleNumberTransition $ inj
  (Proxy :: _ "exponentialRamp")
  unit

_immediately :: AudioSingleNumberTransition
_immediately = AudioSingleNumberTransition $ inj (Proxy :: _ "immediately") unit

derive instance newtypeAudioSingleNumberTransition ::
  Newtype AudioSingleNumberTransition _

derive instance eqAudioSingleNumberTransition :: Eq AudioSingleNumberTransition

instance ordAudioSingleNumberTransition :: Ord AudioSingleNumberTransition where
  compare i0 i1
    | i0 == _immediately = LT
    | i1 == _immediately = GT
    | otherwise = EQ

derive instance genericAudioSingleNumberTransition ::
  Generic AudioSingleNumberTransition _

instance showAudioSingleNumberTransition :: Show AudioSingleNumberTransition where
  show = unwrap >>> match
    { noRamp: const "NoRamp"
    , linearRamp: const "LinearRamp"
    , exponentialRamp: const "ExponentialRamp"
    , immediately: const "Immediately"
    }

instance semigroupAudioSingleNumberTransition ::
  Semigroup AudioSingleNumberTransition where
  append i0 i1
    | i0 == _immediately || i1 == _immediately = _immediately
    | i0 == _exponentialRamp || i1 == _exponentialRamp = _exponentialRamp
    | i0 == _linearRamp || i1 == _linearRamp = _linearRamp
    | otherwise = _noRamp

class Timed a where
  lensTime :: Lens' a Number

singleNumber :: AudioSingleNumber -> AudioParameter
singleNumber = AudioParameter <<< inj
  ( Proxy
      :: Proxy
           "singleNumber"
  )

envelope :: AudioEnvelope -> AudioParameter
envelope = AudioParameter <<< inj
  ( Proxy
      :: Proxy
           "envelope"
  )

cancellation :: AudioParameterCancellation -> AudioParameter
cancellation = AudioParameter <<< inj
  ( Proxy
      :: Proxy
           "cancellation"
  )

instance lensTimeAudioParameter :: Timed AudioParameter where
  lensTime = lens
    ( unwrap >>> match
        { singleNumber: view lensTime
        , envelope: view lensTime
        , cancellation: view lensTime
        }
    )
    ( \(AudioParameter s) a -> match
        { singleNumber: singleNumber <<< set lensTime a
        , envelope: envelope <<< set lensTime a
        , cancellation: cancellation <<< set lensTime a
        }
        s
    )

instance lensTimeAudioSingleNumber :: Timed AudioSingleNumber where
  lensTime = lens (unwrap >>> _.timeOffset)
    (\(AudioSingleNumber s) -> AudioSingleNumber <<< s { timeOffset = _ })

instance lensTimeAudioEnvelope :: Timed AudioEnvelope where
  lensTime = lens (unwrap >>> _.timeOffset)
    (\(AudioEnvelope s) -> AudioEnvelope <<< s { timeOffset = _ })

instance lensTimeAudioParameterCancellation :: Timed AudioParameterCancellation where
  lensTime = lens (unwrap >>> _.timeOffset)
    ( \(AudioParameterCancellation s) ->
        AudioParameterCancellation <<< s { timeOffset = _ }
    )

instance lensTimeAudioOnOff :: Timed AudioOnOff where
  lensTime = lens (unwrap >>> _.timeOffset)
    (\(AudioOnOff s) -> AudioOnOff <<< s { timeOffset = _ })

cancel :: AudioParameterCancellation
cancel = AudioParameterCancellation { hold: false, timeOffset: 0.0 }

ff :: forall a. Timed a => Number -> a -> a
ff = set lensTime

lensRamp :: Lens' AudioSingleNumber AudioSingleNumberTransition
lensRamp = lens (unwrap >>> _.transition)
  (\(AudioSingleNumber s) -> AudioSingleNumber <<< s { transition = _ })

noRamp :: AudioSingleNumber -> AudioSingleNumber
noRamp = set lensRamp _noRamp

linearRamp :: AudioSingleNumber -> AudioSingleNumber
linearRamp = set lensRamp _linearRamp

exponentialRamp :: AudioSingleNumber -> AudioSingleNumber
exponentialRamp = set lensRamp _exponentialRamp

immediately :: AudioSingleNumber -> AudioSingleNumber
immediately = set lensRamp _immediately

class MM a b | a -> b where
  mm :: a -> b

instance maybeMM :: MM (Maybe a) (Maybe a) where
  mm = identity
else instance justMM :: MM a (Maybe a) where
  mm = pure
