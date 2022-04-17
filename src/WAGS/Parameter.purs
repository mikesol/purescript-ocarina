module WAGS.Parameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (over, view)
import Data.Lens.Iso.Newtype (_Newtype, unto)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Variant (Variant, inj, match)
import FRP.Event (class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))

newtype Transition = Transition
  (Variant (linear :: Unit, exponential :: Unit, step :: Unit))

_linear :: Transition
_linear = Transition $ inj (Proxy :: _ "linear") unit

_exponential :: Transition
_exponential = Transition $ inj (Proxy :: _ "exponential") unit

_step :: Transition
_step = Transition $ inj (Proxy :: _ "step") unit

derive instance eqTransition :: Eq Transition
derive instance ordTransition :: Ord Transition
derive instance newtypeTransition :: Newtype Transition _
derive newtype instance showTransition :: Show Transition

_numeric' :: AudioNumeric' -> AudioParameter
_numeric' = _numeric <<< AudioNumeric

_numeric :: AudioNumeric -> AudioParameter
_numeric = AudioParameter <<< inj (Proxy :: _ "numeric")

_envelope :: AudioEnvelope -> AudioParameter
_envelope = AudioParameter <<< inj (Proxy :: _ "envelope")

_envelope' :: AudioEnvelope' -> AudioParameter
_envelope' = _envelope <<< AudioEnvelope

_cancel :: AudioCancel -> AudioParameter
_cancel = AudioParameter <<< inj (Proxy :: _ "cancel")

_cancel' :: AudioCancel' -> AudioParameter
_cancel' = _cancel <<< AudioCancel

_sudden :: AudioSudden -> AudioParameter
_sudden = AudioParameter <<< inj (Proxy :: _ "sudden")

_sudden' :: AudioSudden' -> AudioParameter
_sudden' = _sudden <<< AudioSudden

type AudioNumeric' = { n :: Number, o :: Number, t :: Transition }
newtype AudioNumeric = AudioNumeric AudioNumeric'

derive instance Eq AudioNumeric
derive instance Ord AudioNumeric
derive instance Newtype AudioNumeric _
derive newtype instance Show AudioNumeric

type AudioEnvelope' = { p :: Array Number, o :: Number, d :: Number }
newtype AudioEnvelope = AudioEnvelope AudioEnvelope'

derive instance Eq AudioEnvelope
derive instance Ord AudioEnvelope
derive instance Newtype AudioEnvelope _
derive newtype instance Show AudioEnvelope

type AudioCancel' = { o :: Number }
newtype AudioCancel = AudioCancel AudioCancel'

derive instance Eq AudioCancel
derive instance Ord AudioCancel
derive instance Newtype AudioCancel _
derive newtype instance Show AudioCancel

type AudioSudden' = { n :: Number }
newtype AudioSudden = AudioSudden AudioSudden'

derive instance Eq AudioSudden
derive instance Ord AudioSudden
derive instance Newtype AudioSudden _
derive newtype instance Show AudioSudden

type InitialAudioParameter = Number
newtype AudioParameter = AudioParameter
  ( Variant
      ( numeric :: AudioNumeric
      , envelope :: AudioEnvelope
      , cancel :: AudioCancel
      , sudden :: AudioSudden
      )
  )

derive instance eqAudioParameter :: Eq AudioParameter
derive instance ordAudioParameter :: Ord AudioParameter
derive instance newtypeAudioParameter :: Newtype AudioParameter _
derive newtype instance showAudioParameter :: Show AudioParameter

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

derive instance eqOnOff :: Eq OnOff
derive instance ordOnOff :: Ord OnOff
derive instance newtypeOnOff :: Newtype OnOff _
derive instance genericOnOff :: Generic OnOff _

instance showOnOff :: Show OnOff where
  show = unwrap >>> match
    { on: const "on", off: const "off", offOn: const "offOn" }

newtype AudioOnOff = AudioOnOff
  { n :: OnOff
  , o :: Number
  }

apOn :: AudioOnOff
apOn = AudioOnOff { n: _on, o: 0.0 }

pureOn
  :: forall event nt r
   . IsEvent event
  => Newtype nt (Variant (onOff :: AudioOnOff | r))
  => event nt
pureOn = bang (wrap $ inj (Proxy :: _ "onOff") apOn)

apOff :: AudioOnOff
apOff = AudioOnOff { n: _off, o: 0.0 }

apOffOn :: AudioOnOff
apOffOn = AudioOnOff { n: _offOn, o: 0.0 }

dt
  :: forall nt r
   . Newtype nt { o :: Number | r }
  => (Number -> Number)
  -> nt
  -> nt
dt = over (_Newtype <<< prop (Proxy :: _ "o"))

derive instance eqAudioOnOff :: Eq AudioOnOff
derive instance ordAudioOnOff :: Ord AudioOnOff
derive instance newtypeAudioOnOff :: Newtype AudioOnOff _
derive instance genericAudioOnOff :: Generic AudioOnOff _

type ACTime =
  { concreteTime :: Number, abstractTime :: Number, lookAhead :: Number }

type WriteHead (f :: Type -> Type) = f ACTime

cp :: forall f. Applicative f => Number -> f AudioParameter
cp n = pure (_sudden (AudioSudden { n }))

oo
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> AudioOnOff)
  -> f AudioOnOff
oo wh f = wh # map
  \{ concreteTime, abstractTime } ->
    let
      AudioOnOff { o, n } = f abstractTime
    in
      AudioOnOff { n, o: o + concreteTime }

at
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> Number)
  -> f AudioParameter
at wh f = at' wh (map (\n -> { o: 0.0, n: n }) f)

at'
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> { n :: Number, o :: Number })
  -> f AudioParameter

at' wh f = map _numeric (at_' wh f)

at_
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> Number)
  -> f AudioNumeric
at_ wh f = at_' wh (map (\n -> { o: 0.0, n: n }) f)

at_'
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> { n :: Number, o :: Number })
  -> f AudioNumeric
at_' wh f = wh # map
  \{ concreteTime, abstractTime } ->
    let
      { n, o } = f abstractTime
    in
      AudioNumeric { t: _linear, o: o + concreteTime, n }

uat_
  :: ACTime
  -> (Number -> Number)
  -> AudioNumeric
uat_ wh f = uat_' wh (map (\n -> { o: 0.0, n: n }) f)

uat_'
  :: ACTime
  -> (Number -> { n :: Number, o :: Number })
  -> AudioNumeric
uat_' { concreteTime, abstractTime } f =
  let
    { n, o } = f abstractTime
  in
    AudioNumeric { t: _linear, o: o + concreteTime, n }

ovnn :: (Number -> Number) -> AudioNumeric -> AudioNumeric
ovnn = over (unto AudioNumeric <<< prop (Proxy :: Proxy "n"))

vwnn :: AudioNumeric -> Number
vwnn = view (unto AudioNumeric <<< prop (Proxy :: Proxy "n"))

class ToAudioOnOff i where
  toAudioOnOff :: i -> AudioOnOff

instance ToAudioOnOff Number where
  toAudioOnOff = AudioOnOff <<< { o: _, n: _on }

instance ToAudioOnOff OnOff where
  toAudioOnOff = AudioOnOff <<< { o: 0.0, n: _ }

instance ToAudioOnOff AudioOnOff where
  toAudioOnOff = identity

class ToAudioParameter i where
  toAudioParameter :: i -> AudioParameter

instance ToAudioParameter Number where
  toAudioParameter n = _sudden (AudioSudden { n })

instance ToAudioParameter AudioParameter where
  toAudioParameter = identity

instance ToAudioParameter AudioNumeric where
  toAudioParameter = _numeric

instance ToAudioParameter AudioSudden where
  toAudioParameter = _sudden

instance ToAudioParameter AudioCancel where
  toAudioParameter = _cancel

instance ToAudioParameter AudioEnvelope where
  toAudioParameter = _envelope