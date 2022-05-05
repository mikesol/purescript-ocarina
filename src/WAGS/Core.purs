module WAGS.Core where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold, for_, oneOfMap, traverse_)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lens (Optic', over)
import Data.Lens.Iso.Newtype (_Newtype, unto)
import Data.Lens.Record (prop)
import Data.Maybe as DM
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong (class Strong)
import Data.Show.Generic (genericShow)
import Data.Typelevel.Num (D1)
import Data.Variant (Variant, inj, match)
import Data.Variant.Maybe (Maybe)
import Data.Vec (Vec)
import Effect (Effect)
import Effect.AVar (tryPut)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import Effect.Ref as Ref
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons)
import Simple.JSON as JSON
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

-- start param
newtype Transition = Transition
  (Variant (linear :: Unit, exponential :: Unit, step :: Unit))

_linear :: Transition
_linear = Transition $ inj (Proxy :: _ "linear") unit

_exponential :: Transition
_exponential = Transition $ inj (Proxy :: _ "exponential") unit

_step :: Transition
_step = Transition $ inj (Proxy :: _ "step") unit

_numeric' :: forall lock payload. AudioNumeric' -> AudioParameter lock payload
_numeric' = _numeric <<< AudioNumeric

_numeric :: forall lock payload. AudioNumeric -> AudioParameter lock payload
_numeric = AudioParameter <<< inj (Proxy :: _ "numeric")

_unit' :: forall lock payload. AudioUnit' lock payload -> AudioParameter lock payload
_unit' = _unit <<< AudioUnit

_unit :: forall lock payload. AudioUnit lock payload -> AudioParameter lock payload
_unit = AudioParameter <<< inj (Proxy :: _ "unit")

_envelope :: forall lock payload. AudioEnvelope -> AudioParameter lock payload
_envelope = AudioParameter <<< inj (Proxy :: _ "envelope")

_envelope' :: forall lock payload. AudioEnvelope' -> AudioParameter lock payload
_envelope' = _envelope <<< AudioEnvelope

_cancel :: forall lock payload. AudioCancel -> AudioParameter lock payload
_cancel = AudioParameter <<< inj (Proxy :: _ "cancel")

_cancel' :: forall lock payload. AudioCancel' -> AudioParameter lock payload
_cancel' = _cancel <<< AudioCancel

_sudden :: forall lock payload. AudioSudden -> AudioParameter lock payload
_sudden = AudioParameter <<< inj (Proxy :: _ "sudden")

_sudden' :: forall lock payload. AudioSudden' -> AudioParameter lock payload
_sudden' = _sudden <<< AudioSudden

type AudioNumeric' = { n :: Number, o :: Number, t :: Transition }
newtype AudioNumeric = AudioNumeric AudioNumeric'

derive instance Newtype AudioNumeric _

type AudioEnvelope' = { p :: Array Number, o :: Number, d :: Number }
newtype AudioEnvelope = AudioEnvelope AudioEnvelope'

derive instance Newtype AudioEnvelope _

type AudioCancel' = { o :: Number }
newtype AudioCancel = AudioCancel AudioCancel'

derive instance Newtype AudioCancel _

type AudioUnit' lock payload = { u :: Node D1 lock payload }
newtype AudioUnit lock payload = AudioUnit (AudioUnit' lock payload)

type AudioSudden' = { n :: Number }
newtype AudioSudden = AudioSudden AudioSudden'

derive instance Newtype AudioSudden _

type InitialAudioParameter = Number

newtype AudioParameter lock payload = AudioParameter
  ( Variant
      ( numeric :: AudioNumeric
      , envelope :: AudioEnvelope
      , cancel :: AudioCancel
      , sudden :: AudioSudden
      , unit :: AudioUnit lock payload
      )
  )

type FFIAudioUnit' = { i :: String }
newtype FFIAudioUnit = FFIAudioUnit FFIAudioUnit'

derive instance Newtype FFIAudioUnit _

newtype FFIAudioParameter = FFIAudioParameter
  ( Variant
      ( numeric :: AudioNumeric
      , envelope :: AudioEnvelope
      , cancel :: AudioCancel
      , sudden :: AudioSudden
      , unit :: FFIAudioUnit
      )
  )

-- | Term-level constructor for a generator being on or off
newtype OnOff = OnOff
  ( Variant
      ( on :: Unit
      , off :: Unit
      )
  )

_on :: OnOff
_on = OnOff $ inj (Proxy :: _ "on") unit

_off :: OnOff
_off = OnOff $ inj (Proxy :: _ "off") unit

derive instance eqOnOff :: Eq OnOff
derive instance ordOnOff :: Ord OnOff
derive instance newtypeOnOff :: Newtype OnOff _
derive instance genericOnOff :: Generic OnOff _

instance showOnOff :: Show OnOff where
  show = unwrap >>> match
    { on: const "on", off: const "off" }

newtype AudioOnOff = AudioOnOff
  { x :: OnOff
  , o :: Number
  }

apOn :: AudioOnOff
apOn = AudioOnOff { x: _on, o: 0.0 }

bangOn
  :: forall nt r
   . Newtype nt (Variant (onOff :: AudioOnOff | r))
  => Event nt
bangOn = bang (wrap $ inj (Proxy :: _ "onOff") apOn)

apOff :: AudioOnOff
apOff = AudioOnOff { x: _off, o: 0.0 }

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

class ToAudioOnOff i where
  toAudioOnOff :: i -> AudioOnOff

instance ToAudioOnOff Number where
  toAudioOnOff = AudioOnOff <<< { o: _, x: _on }

instance ToAudioOnOff OnOff where
  toAudioOnOff = AudioOnOff <<< { o: 0.0, x: _ }

instance ToAudioOnOff AudioOnOff where
  toAudioOnOff = identity

class ToAudioParameter i l p where
  toAudioParameter :: i -> AudioParameter l p

instance ToAudioParameter Number l p where
  toAudioParameter n = _sudden (AudioSudden { n })

instance (TypeEquals l0 l1, TypeEquals p0 p1) => ToAudioParameter (AudioParameter l0 p0) l1 p1 where
  toAudioParameter = (unsafeCoerce :: AudioParameter l0 p0 -> AudioParameter l1 p1)

instance ToAudioParameter AudioNumeric l p where
  toAudioParameter = _numeric

instance ToAudioParameter AudioSudden l p where
  toAudioParameter = _sudden

instance ToAudioParameter AudioCancel l p where
  toAudioParameter = _cancel

instance ToAudioParameter AudioEnvelope l p where
  toAudioParameter = _envelope

instance (TypeEquals l0 l1, TypeEquals p0 p1) => ToAudioParameter (AudioUnit l0 p0) l1 p1 where
  toAudioParameter = _unit <<< (unsafeCoerce :: AudioUnit l0 p0 -> AudioUnit l1 p1)

instance (TypeEquals l0 l1, TypeEquals p0 p1) => ToAudioParameter (Node D1 l0 p0) l1 p1 where
  toAudioParameter = toAudioParameter <<< AudioUnit <<< { u: _ }

c1 :: forall l p. (forall o. Node o l p) -> Node D1 l p
c1 (Node o) = Node o

class OpticN s where
  opticN :: forall p. Strong p => Optic' p s Number

instance (Cons "n" Number r' r) => OpticN AudioNumeric where
  opticN = unto AudioNumeric <<< prop (Proxy :: _ "n")

instance (Cons "n" Number r' r) => OpticN AudioSudden where
  opticN = unto AudioSudden <<< prop (Proxy :: _ "n")

class OpticO s where
  opticO :: forall p. Strong p => Optic' p s Number

instance (Cons "n" Number r' r) => OpticO AudioOnOff where
  opticO = unto AudioOnOff <<< prop (Proxy :: _ "o")

instance (Cons "n" Number r' r) => OpticO AudioNumeric where
  opticO = unto AudioNumeric <<< prop (Proxy :: _ "o")

instance (Cons "n" Number r' r) => OpticO AudioEnvelope where
  opticO = unto AudioEnvelope <<< prop (Proxy :: _ "o")

instance (Cons "n" Number r' r) => OpticO AudioCancel where
  opticO = unto AudioCancel <<< prop (Proxy :: _ "o")

-----------
-- end param
--
type AudioWorkletNodeOptions_' param =
  { name :: String
  , numberOfInputs :: Int
  , numberOfOutputs :: Int
  , outputChannelCount :: Array Int
  , parameterData :: Object param
  , processorOptions :: Foreign
  }

type AudioWorkletNodeOptions_S' param =
  { name :: String
  , numberOfInputs :: Int
  , numberOfOutputs :: Int
  , outputChannelCount :: Array Int
  , parameterData :: Object param
  , processorOptions :: String
  }

audioWorkletNodeOptionsForInstances
  :: AudioWorkletNodeOptions_' ~> AudioWorkletNodeOptions_S'
audioWorkletNodeOptionsForInstances
  { name
  , numberOfInputs
  , numberOfOutputs
  , outputChannelCount
  , parameterData
  , processorOptions
  } =
  { name
  , numberOfInputs
  , numberOfOutputs
  , outputChannelCount
  , parameterData
  , processorOptions: JSON.writeJSON processorOptions
  }

derive instance newtypeAudioWorkletNodeOptions_ ::
  Newtype (AudioWorkletNodeOptions_ param) _

newtype AudioWorkletNodeOptions_ param = AudioWorkletNodeOptions_
  { name :: String
  , numberOfInputs :: Int
  , numberOfOutputs :: Int
  , outputChannelCount :: Array Int
  , parameterData :: Object param
  , processorOptions :: Foreign
  }

instance eqAudioWorkletNodeOptions_ ::
  Eq param =>
  Eq (AudioWorkletNodeOptions_ param) where
  eq = eq `on` (unwrap >>> audioWorkletNodeOptionsForInstances)

instance ordAudioWorkletNodeOptions_ ::
  Ord param =>
  Ord (AudioWorkletNodeOptions_ param) where
  compare = compare `on` (unwrap >>> audioWorkletNodeOptionsForInstances)

instance showAudioWorkletNodeOptions_ ::
  Show param =>
  Show (AudioWorkletNodeOptions_ param) where
  show (AudioWorkletNodeOptions_ a) = "AudioWorkletNodeOptions < "
    <> a.name
    <> ", "
    <> show a.numberOfInputs
    <> ", "
    <> show a.numberOfOutputs
    <> ", "
    <> show a.outputChannelCount
    <> ", "
    <> show a.parameterData
    <> ", "
    <> JSON.writeJSON a.numberOfInputs
    <> " >"

newtype DynamicChannels outputChannels lock payload = DynamicChannels
  (Event (Event (Channel outputChannels lock payload)))

newtype FixedChannels outputChannels lock payload = FixedChannels
  (Array (Node outputChannels lock payload))

newtype EventfulNode outputChannels lock payload = EventfulNode
  (Event (Audible outputChannels lock payload))

type Node' :: forall k. k -> Type -> Type
type Node' lock payload =
  { parent :: Maybe String
  , scope :: String
  , raiseId :: String -> Effect Unit
  }
  -> AudioInterpret payload
  -> Event payload

newtype Node :: Type -> Type -> Type -> Type
newtype Node outputChannels lock payload = Node (Node' lock payload)

data Channel outputChannels lock payload
  = Sound (Node outputChannels lock payload)
  | Silence

newtype Audible outputChannels lock payload = Audible
  ( Variant
      ( dynamicChannels :: DynamicChannels outputChannels lock payload
      , fixedChannels :: FixedChannels outputChannels lock payload
      , eventfulNode :: EventfulNode outputChannels lock payload
      , node :: Node outputChannels lock payload
      )
  )

--
class Mix sound board | sound -> board where
  mix :: sound -> board

instance
  ( TypeEquals outputChannelsi outputChannelso
  , TypeEquals locki locko
  , TypeEquals payloadi payloado
  ) =>
  Mix (Audible outputChannelsi locki payloadi)
    (Audible outputChannelso locko payloado) where
  mix = unsafeCoerce :: Audible _ _ _ -> Audible _ _ _

instance
  ( TypeEquals outputChannelsi outputChannelso
  , TypeEquals locki locko
  , TypeEquals payloadi payloado
  ) =>
  Mix (Event (Event (Channel outputChannelsi locki payloadi)))
    (Audible outputChannelso locko payloado) where
  mix i = Audible $ inj (Proxy :: _ "dynamicChannels")
    (DynamicChannels ((unsafeCoerce :: (Event (Event (Channel _ _ _))) -> (Event (Event (Channel _ _ _)))) i))

instance
  ( TypeEquals outputChannelsi outputChannelso
  , TypeEquals locki locko
  , TypeEquals payloadi payloado
  ) =>
  Mix (Event (Audible outputChannelsi locki payloadi))
    (Audible outputChannelso locko payloado) where
  mix i = Audible $ inj (Proxy :: _ "eventfulNode") (EventfulNode ((unsafeCoerce :: Event (Audible _ _ _) -> Event (Audible _ _ _)) i))

instance
  ( TypeEquals outputChannelsi outputChannelso
  , TypeEquals locki locko
  , TypeEquals payloadi payloado
  ) =>
  Mix (Node outputChannelsi locki payloadi) (Audible outputChannelso locko payloado) where
  mix i = Audible $ inj (Proxy :: _ "node") ((unsafeCoerce :: Node _ _ _ -> Node _ _ _) i)

instance
  ( TypeEquals outputChannelsi outputChannelso
  , TypeEquals locki locko
  , TypeEquals payloadi payloado
  ) =>
  Mix (Array (Node outputChannelsi locki payloadi))
    (Audible outputChannelso locko payloado) where
  mix i = Audible $ inj (Proxy :: _ "fixedChannels") (FixedChannels ((unsafeCoerce :: Array (Node _ _ _) -> Array (Node _ _ _)) i))

--

newtype RealImg = RealImg { real :: Array Number, img :: Array Number }

derive instance newtypeRealImg :: Newtype RealImg _
derive instance eqRealImg :: Eq RealImg
derive instance ordRealImg :: Ord RealImg
derive newtype instance showRealImg :: Show RealImg

newtype PeriodicOscSpec = PeriodicOscSpec
  ( Variant
      ( wave :: BrowserPeriodicWave
      , realImg :: RealImg
      )
  )

_wave :: BrowserPeriodicWave -> PeriodicOscSpec
_wave = PeriodicOscSpec <<< inj (Proxy :: Proxy "wave")

_realImg :: RealImg -> PeriodicOscSpec
_realImg = PeriodicOscSpec <<< inj (Proxy :: Proxy "realImg")

derive instance newtypePeriodicOscSpec :: Newtype PeriodicOscSpec _
derive newtype instance eqPeriodicOscSpec :: Eq PeriodicOscSpec
derive newtype instance ordPeriodicOscSpec :: Ord PeriodicOscSpec
derive newtype instance showPeriodicOscSpec :: Show PeriodicOscSpec

newtype Oversample = Oversample
  ( Variant
      ( none :: Unit
      , "2x" :: Unit
      , "4x" :: Unit
      )
  )

derive instance newtypeOversample :: Newtype Oversample _

_none :: Oversample
_none = Oversample (inj (Proxy :: Proxy "none") unit)

_twoX :: Oversample
_twoX = Oversample (inj (Proxy :: Proxy "2x") unit)

_fourX :: Oversample
_fourX = Oversample (inj (Proxy :: Proxy "4x") unit)

derive instance eqOversample :: Eq Oversample
derive instance ordOversample :: Ord Oversample
derive instance genericOversample :: Generic Oversample _
instance showOversample :: Show Oversample where
  show = genericShow

--

type ConnectXToY_ = (from :: String, to :: String)
type ConnectXToY = { | ConnectXToY_ }
type ConnectXToY' = { fromUnit :: String, toUnit :: String | ConnectXToY_ }
type DisconnectXFromY_ = (from :: String, to :: String)
type DisconnectXFromY = { | DisconnectXFromY_ }
type DisconnectXFromY' =
  { fromUnit :: String, toUnit :: String | DisconnectXFromY_ }

type DeleteFromCache = { id :: String }

derive instance newtypeAllpass :: Newtype (Allpass lock parameter) _
newtype Allpass lock parameter = Allpass
  ( Variant
      ( frequency :: AudioParameter lock parameter
      , q :: AudioParameter lock parameter
      )
  )

derive instance newtypeInitializeAllpass :: Newtype InitializeAllpass _
newtype InitializeAllpass = InitializeAllpass
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }

type MakeAllpass_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  , q :: param
  }

type MakeAllpass = MakeAllpass_ InitialAudioParameter
type MakeAllpass' lock payload = MakeAllpass_ (AudioParameter lock payload)

derive instance newtypeAnalyser :: Newtype Analyser _
newtype Analyser = Analyser (Variant (cb :: AnalyserNodeCb))

data Po2 = TTT7 | TTT8 | TTT9 | TTT10 | TTT11 | TTT12 | TTT13
data ChannelCountMode = ClampedMax | Max | Explicit
data ChannelInterpretation = Speakers | Discrete

derive instance newtypeInitializeAnalyser :: Newtype InitializeAnalyser _
newtype InitializeAnalyser = InitializeAnalyser
  { cb :: AnalyserNodeCb
  , fftSize :: Po2
  , maxDecibels :: Number
  , minDecibels :: Number
  , smoothingTimeConstant :: Number
  , channelCount :: Int
  , channelCountMode :: ChannelCountMode
  , channelInterpretation :: ChannelInterpretation
  }

type MakeAnalyser =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , cb :: AnalyserNodeCb
  , fftSize :: Int
  , maxDecibels :: Number
  , minDecibels :: Number
  , smoothingTimeConstant :: Number
  , channelCount :: Int
  , channelCountMode :: String
  , channelInterpretation :: String
  }

derive instance newtypeAudioWorkletNode ::
  Newtype (AudioWorkletNode parameterData) _

newtype AudioWorkletNode parameterData = AudioWorkletNode
  (Variant parameterData)

derive instance newtypeInitializeAudioWorkletNode ::
  Newtype ( InitializeAudioWorkletNode name numberOfInputs
        numberOfOutputs
        outputChannelCount
        parameterData
        processorOptions
    )
    _

newtype InitializeAudioWorkletNode
  (name :: Symbol)
  numberOfInputs
  numberOfOutputs
  outputChannelCount
  parameterData
  processorOptions = InitializeAudioWorkletNode
  { name :: Proxy name
  , numberOfInputs :: numberOfInputs
  , numberOfOutputs :: numberOfOutputs
  , outputChannelCount :: outputChannelCount
  , parameterData :: { | parameterData }
  , processorOptions :: { | processorOptions }
  }

type MakeAudioWorkletNode_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , options :: AudioWorkletNodeOptions_ param
  }

type MakeAudioWorkletNode = MakeAudioWorkletNode_ InitialAudioParameter
type MakeAudioWorkletNode' lock payload = MakeAudioWorkletNode_ (AudioParameter lock payload)

derive instance newtypeBandpass :: Newtype (Bandpass lock payload) _
newtype Bandpass lock payload = Bandpass
  ( Variant
      ( frequency :: AudioParameter lock payload
      , q :: AudioParameter lock payload
      )
  )

derive instance newtypeInitializeBandpass :: Newtype InitializeBandpass _
newtype InitializeBandpass = InitializeBandpass
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }

type MakeBandpass_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  , q :: param
  }

type MakeBandpass = MakeBandpass_ InitialAudioParameter
type MakeBandpass' lock payload = MakeBandpass_ (AudioParameter lock payload)

derive instance newtypeConstant :: Newtype (Constant lock payload) _
newtype Constant lock payload = Constant
  (Variant (offset :: AudioParameter lock payload, onOff :: AudioOnOff))

derive instance newtypeInitializeConstant :: Newtype InitializeConstant _
newtype InitializeConstant = InitializeConstant
  { offset :: InitialAudioParameter }

type MakeConstant_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: String
  , offset :: param
  )

type MakeConstant = { | MakeConstant_ InitialAudioParameter }
type MakeConstant' lock payload = { onOff :: AudioOnOff | MakeConstant_ (AudioParameter lock payload) }

derive instance newtypeInitializeConvolver :: Newtype InitializeConvolver _
newtype InitializeConvolver = InitializeConvolver
  { buffer :: BrowserAudioBuffer }

type MakeConvolver =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , buffer :: BrowserAudioBuffer
  }

derive instance newtypeDelay :: Newtype (Delay lock payload) _
newtype Delay lock payload = Delay (Variant (delayTime :: AudioParameter lock payload))

derive instance newtypeInitializeDelay :: Newtype InitializeDelay _
newtype InitializeDelay = InitializeDelay { delayTime :: InitialAudioParameter, maxDelayTime :: InitialAudioParameter }
type MakeDelay_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , delayTime :: param
  , maxDelayTime :: Number
  }

type MakeDelay = MakeDelay_ InitialAudioParameter
type MakeDelay' lock payload = MakeDelay_ (AudioParameter lock payload)

derive instance newtypeDynamicsCompressor :: Newtype (DynamicsCompressor lock payload) _
newtype DynamicsCompressor lock payload = DynamicsCompressor
  ( Variant
      ( threshold :: AudioParameter lock payload
      , knee :: AudioParameter lock payload
      , ratio :: AudioParameter lock payload
      , attack :: AudioParameter lock payload
      , release :: AudioParameter lock payload
      )
  )

derive instance newtypeInitializeDynamicsCompressor ::
  Newtype InitializeDynamicsCompressor _

newtype InitializeDynamicsCompressor = InitializeDynamicsCompressor
  { threshold :: InitialAudioParameter
  , knee :: InitialAudioParameter
  , ratio :: InitialAudioParameter
  , attack :: InitialAudioParameter
  , release :: InitialAudioParameter
  }

type MakeDynamicsCompressor_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , threshold :: param
  , knee :: param
  , ratio :: param
  , attack :: param
  , release :: param
  }

type MakeDynamicsCompressor = MakeDynamicsCompressor_ InitialAudioParameter
type MakeDynamicsCompressor' lock payload = MakeDynamicsCompressor_ (AudioParameter lock payload)

derive instance newtypeGain :: Newtype (Gain lock payload) _
newtype Gain lock payload = Gain (Variant (gain :: AudioParameter lock payload))

derive instance newtypeInitializeGain :: Newtype InitializeGain _
newtype InitializeGain = InitializeGain { gain :: InitialAudioParameter }
type MakeGain_ param =
  { id :: String, parent :: Maybe String, scope :: String, gain :: param }

type MakeGain = MakeGain_ InitialAudioParameter
type MakeGain' lock payload = MakeGain_ (AudioParameter lock payload)

derive instance newtypeHighpass :: Newtype (Highpass lock payload) _
newtype Highpass lock payload = Highpass
  ( Variant
      ( frequency :: AudioParameter lock payload
      , q :: AudioParameter lock payload
      )
  )

derive instance newtypeInitializeHighpass :: Newtype InitializeHighpass _
newtype InitializeHighpass = InitializeHighpass
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }

type MakeHighpass_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  , q :: param
  }

type MakeHighpass = MakeHighpass_ InitialAudioParameter
type MakeHighpass' lock payload = MakeHighpass_ (AudioParameter lock payload)

derive instance newtypeHighshelf :: Newtype (Highshelf lock payload) _
newtype Highshelf lock payload = Highshelf
  ( Variant
      ( frequency :: AudioParameter lock payload
      , gain :: AudioParameter lock payload
      )
  )

derive instance newtypeInitializeHighshelf :: Newtype InitializeHighshelf _
newtype InitializeHighshelf = InitializeHighshelf
  { frequency :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }

type MakeHighshelf_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  , gain :: param
  }

type MakeHighshelf = MakeHighshelf_ InitialAudioParameter
type MakeHighshelf' lock payload = MakeHighshelf_ (AudioParameter lock payload)

derive instance newtypeInitializeIIRFilter :: Newtype (InitializeIIRFilter feedforward feedback) _
newtype InitializeIIRFilter (feedforward :: Type) (feedback :: Type) = InitializeIIRFilter
  { feedforward :: Vec feedforward Number, feedback :: Vec feedback Number }

type MakeIIRFilter =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , feedforward :: Array Number
  , feedback :: Array Number
  }

derive instance newtypeLoopBuf :: Newtype (LoopBuf lock payload) _
newtype LoopBuf lock payload = LoopBuf
  ( Variant
      ( buffer :: BrowserAudioBuffer
      , onOff :: AudioOnOff
      , playbackRate :: AudioParameter lock payload
      , loopStart :: Number
      , loopEnd :: Number
      )
  )

derive instance newtypeInitializeLoopBuf :: Newtype InitializeLoopBuf _
newtype InitializeLoopBuf = InitializeLoopBuf
  { buffer :: BrowserAudioBuffer
  , playbackRate :: InitialAudioParameter
  , loopStart :: Number
  , loopEnd :: Number
  -- duration in initializer but not setter for now
  , duration :: Maybe Number
  }

type MakeLoopBuf_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: String
  , buffer :: BrowserAudioBuffer
  , playbackRate :: param
  , loopStart :: Number
  , loopEnd :: Number
  , duration :: Maybe Number
  )

type MakeLoopBuf = { | MakeLoopBuf_ InitialAudioParameter }
type MakeLoopBuf' lock payload = { onOff :: AudioOnOff | MakeLoopBuf_ (AudioParameter lock payload) }

derive instance newtypeLowpass :: Newtype (Lowpass lock payload) _
newtype Lowpass lock payload = Lowpass
  ( Variant
      ( frequency :: AudioParameter lock payload
      , q :: AudioParameter lock payload
      )
  )

derive instance newtypeInitializeLowpass :: Newtype InitializeLowpass _
newtype InitializeLowpass = InitializeLowpass
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }

type MakeLowpass_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  , q :: param
  }

type MakeLowpass = MakeLowpass_ InitialAudioParameter
type MakeLowpass' lock payload = MakeLowpass_ (AudioParameter lock payload)

derive instance newtypeLowshelf :: Newtype (Lowshelf lock payload) _
newtype Lowshelf lock payload = Lowshelf
  ( Variant
      ( frequency :: AudioParameter lock payload
      , gain :: AudioParameter lock payload
      )
  )

derive instance newtypeInitializeLowshelf :: Newtype InitializeLowshelf _
newtype InitializeLowshelf = InitializeLowshelf
  { frequency :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }

type MakeLowshelf_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  , gain :: param
  }

type MakeLowshelf = MakeLowshelf_ InitialAudioParameter
type MakeLowshelf' lock payload = MakeLowshelf_ (AudioParameter lock payload)

derive instance newtypeInitializeMediaElement ::
  Newtype InitializeMediaElement _

newtype InitializeMediaElement = InitializeMediaElement
  { element :: BrowserMediaElement
  }

type MakeMediaElement =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , element :: BrowserMediaElement
  }

derive instance newtypeInitializeMicrophone :: Newtype InitializeMicrophone _
newtype InitializeMicrophone = InitializeMicrophone
  { microphone :: BrowserMicrophone
  }

type MakeMicrophone =
  { id :: String
  , microphone :: BrowserMicrophone
  , parent :: Maybe String
  , scope :: String
  }

derive instance newtypeNotch :: Newtype (Notch lock payload) _
newtype Notch lock payload = Notch
  ( Variant
      ( frequency :: AudioParameter lock payload
      , q :: AudioParameter lock payload
      )
  )

derive instance newtypeInitializeNotch :: Newtype InitializeNotch _
newtype InitializeNotch = InitializeNotch
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }

type MakeNotch_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  , q :: param
  }

type MakeNotch = MakeNotch_ InitialAudioParameter
type MakeNotch' lock payload = MakeNotch_ (AudioParameter lock payload)

derive instance newtypePeaking :: Newtype (Peaking lock payload) _
newtype Peaking lock payload = Peaking
  ( Variant
      ( frequency :: AudioParameter lock payload
      , q :: AudioParameter lock payload
      , gain :: AudioParameter lock payload
      )
  )

derive instance newtypeInitializePeaking :: Newtype InitializePeaking _
newtype InitializePeaking = InitializePeaking
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }

type MakePeaking_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  , q :: param
  , gain :: param
  }

type MakePeaking = MakePeaking_ InitialAudioParameter
type MakePeaking' lock payload = MakePeaking_ (AudioParameter lock payload)

derive instance newtypePeriodicOsc :: Newtype (PeriodicOsc lock payload) _
newtype PeriodicOsc lock payload =
  PeriodicOsc
    ( Variant
        ( spec :: PeriodicOscSpec
        , onOff :: AudioOnOff
        , frequency :: AudioParameter lock payload
        )
    )

derive instance newtypeInitializePeriodicOsc :: Newtype InitializePeriodicOsc _
newtype InitializePeriodicOsc = InitializePeriodicOsc
  { spec :: PeriodicOscSpec
  , frequency :: InitialAudioParameter
  }

type MakePeriodicOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: String
  , spec :: PeriodicOscSpec
  , frequency :: param
  )

type MakePeriodicOsc = { | MakePeriodicOsc_ InitialAudioParameter }
type MakePeriodicOsc' lock payload =
  { onOff :: AudioOnOff | MakePeriodicOsc_ (AudioParameter lock payload) }

derive instance newtypePlayBuf :: Newtype (PlayBuf lock payload) _
newtype PlayBuf lock payload =
  PlayBuf
    ( Variant
        ( buffer :: BrowserAudioBuffer
        , bufferOffset :: Number
        , onOff :: AudioOnOff
        , playbackRate :: AudioParameter lock payload
        , duration :: Maybe Number
        )
    )

derive instance newtypeInitializePlayBuf :: Newtype InitializePlayBuf _
newtype InitializePlayBuf = InitializePlayBuf
  { buffer :: BrowserAudioBuffer
  , bufferOffset :: Number
  , playbackRate :: InitialAudioParameter
  , duration :: Maybe Number
  }

type MakePlayBuf_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: String
  , buffer :: BrowserAudioBuffer
  , bufferOffset :: Number
  , playbackRate :: param
  , duration :: Maybe Number
  )

type MakePlayBuf = { | MakePlayBuf_ InitialAudioParameter }
type MakePlayBuf' lock payload = { onOff :: AudioOnOff | MakePlayBuf_ (AudioParameter lock payload) }

derive instance newtypeInitializeRecorder :: Newtype InitializeRecorder _
newtype InitializeRecorder = InitializeRecorder { cb :: MediaRecorderCb }
type MakeRecorder =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , cb :: MediaRecorderCb
  }

type MakeSpeaker = { id :: String }

derive instance newtypeSawtoothOsc :: Newtype (SawtoothOsc lock payload) _
newtype SawtoothOsc lock payload =
  SawtoothOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter lock payload
        )
    )

derive instance newtypeInitializeSawtoothOsc :: Newtype InitializeSawtoothOsc _
newtype InitializeSawtoothOsc = InitializeSawtoothOsc
  { frequency :: InitialAudioParameter
  }

type MakeSawtoothOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  )

type MakeSawtoothOsc = { | MakeSawtoothOsc_ InitialAudioParameter }
type MakeSawtoothOsc' lock payload =
  { onOff :: AudioOnOff | MakeSawtoothOsc_ (AudioParameter lock payload) }

derive instance newtypeSinOsc :: Newtype (SinOsc lock payload) _
newtype SinOsc lock payload = SinOsc
  (Variant (frequency :: AudioParameter lock payload, onOff :: AudioOnOff))

derive instance newtypeInitializeSinOsc :: Newtype InitializeSinOsc _
newtype InitializeSinOsc = InitializeSinOsc
  { frequency :: InitialAudioParameter
  }

type MakeSinOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  )

type MakeSinOsc = { | MakeSinOsc_ InitialAudioParameter }
type MakeSinOsc' lock payload = { onOff :: AudioOnOff | MakeSinOsc_ (AudioParameter lock payload) }

derive instance newtypeSquareOsc :: Newtype (SquareOsc lock payload) _
newtype SquareOsc lock payload =
  SquareOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter lock payload
        )
    )

derive instance newtypeInitializeSquareOsc :: Newtype InitializeSquareOsc _
newtype InitializeSquareOsc = InitializeSquareOsc
  { frequency :: InitialAudioParameter
  }

type MakeSquareOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  )

type MakeSquareOsc = { | MakeSquareOsc_ InitialAudioParameter }
type MakeSquareOsc' lock payload = { onOff :: AudioOnOff | MakeSquareOsc_ (AudioParameter lock payload) }

derive instance newtypeStereoPanner :: Newtype (StereoPanner lock payload) _
newtype StereoPanner lock payload =
  StereoPanner (Variant (pan :: AudioParameter lock payload))

derive instance newtypeInitializeStereoPanner ::
  Newtype InitializeStereoPanner _

newtype InitializeStereoPanner = InitializeStereoPanner
  { pan :: InitialAudioParameter }

type MakeStereoPanner_ param =
  { id :: String, parent :: Maybe String, scope :: String, pan :: param }

type MakeStereoPanner = MakeStereoPanner_ InitialAudioParameter
type MakeStereoPanner' lock payload = MakeStereoPanner_ (AudioParameter lock payload)

derive instance newtypeTriangleOsc :: Newtype (TriangleOsc lock parameter) _
newtype TriangleOsc lock parameter =
  TriangleOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter lock parameter
        )
    )

derive instance newtypeInitializeTriangleOsc :: Newtype InitializeTriangleOsc _
newtype InitializeTriangleOsc = InitializeTriangleOsc
  { frequency :: InitialAudioParameter
  }

type MakeTriangleOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: String
  , frequency :: param
  )

type MakeTriangleOsc = { | MakeTriangleOsc_ InitialAudioParameter }
type MakeTriangleOsc' lock payload =
  { onOff :: AudioOnOff | MakeTriangleOsc_ (AudioParameter lock payload) }

derive instance newtypeInitializeWaveShaper :: Newtype InitializeWaveShaper _
newtype InitializeWaveShaper = InitializeWaveShaper
  { curve :: BrowserFloatArray
  , oversample :: Oversample
  }

type MakeWaveShaper =
  { id :: String
  , parent :: Maybe String
  , scope :: String
  , curve :: BrowserFloatArray
  , oversample :: Oversample
  }

type SetAnalyserNodeCb = { id :: String, cb :: AnalyserNodeCb }
type SetMediaRecorderCb = { id :: String, cb :: MediaRecorderCb }
type SetAudioWorkletParameter =
  { id :: String, paramName :: String, paramValue :: FFIAudioParameter }

type SetBuffer = { id :: String, buffer :: BrowserAudioBuffer }
type SetConvolverBuffer = { id :: String, buffer :: BrowserAudioBuffer }
type SetPeriodicOsc = { id :: String, spec :: PeriodicOscSpec }
type SetOnOff = { id :: String, onOff :: AudioOnOff }
type SetBufferOffset = { id :: String, bufferOffset :: Number }
type SetDuration = { id :: String, duration :: Maybe Number }
type SetLoopStart = { id :: String, loopStart :: Number }
type SetLoopEnd = { id :: String, loopEnd :: Number }
type SetRatio = { id :: String, ratio :: FFIAudioParameter }
type SetOffset = { id :: String, offset :: FFIAudioParameter }
type SetAttack = { id :: String, attack :: FFIAudioParameter }
type SetGain = { id :: String, gain :: FFIAudioParameter }
type SetQ = { id :: String, q :: FFIAudioParameter }
type SetPan = { id :: String, pan :: FFIAudioParameter }
type SetThreshold = { id :: String, threshold :: FFIAudioParameter }
type SetRelease = { id :: String, release :: FFIAudioParameter }
type SetKnee = { id :: String, knee :: FFIAudioParameter }
type SetDelay = { id :: String, delayTime :: FFIAudioParameter }
type SetPlaybackRate = { id :: String, playbackRate :: FFIAudioParameter }
type SetFrequency = { id :: String, frequency :: FFIAudioParameter }
type SetWaveShaperCurve = { id :: String, curve :: BrowserFloatArray }

newtype AudioInterpret payload = AudioInterpret
  { ids :: Effect String
  , disconnectXFromY :: DisconnectXFromY -> payload
  , connectXToY :: ConnectXToY -> payload
  , deleteFromCache :: DeleteFromCache -> payload
  , makeAllpass :: MakeAllpass -> payload
  , makeAnalyser :: MakeAnalyser -> payload
  , makeAudioWorkletNode :: MakeAudioWorkletNode -> payload
  , makeBandpass :: MakeBandpass -> payload
  , makeConstant :: MakeConstant -> payload
  , makeConvolver :: MakeConvolver -> payload
  , makeDelay :: MakeDelay -> payload
  , makeDynamicsCompressor :: MakeDynamicsCompressor -> payload
  , makeGain :: MakeGain -> payload
  , makeHighpass :: MakeHighpass -> payload
  , makeHighshelf :: MakeHighshelf -> payload
  , makeIIRFilter :: MakeIIRFilter -> payload
  , makeLoopBuf :: MakeLoopBuf -> payload
  , makeLowpass :: MakeLowpass -> payload
  , makeLowshelf :: MakeLowshelf -> payload
  , makeMediaElement :: MakeMediaElement -> payload
  , makeMicrophone :: MakeMicrophone -> payload
  , makeNotch :: MakeNotch -> payload
  , makePeaking :: MakePeaking -> payload
  , makePeriodicOsc :: MakePeriodicOsc -> payload
  , makePlayBuf :: MakePlayBuf -> payload
  , makeRecorder :: MakeRecorder -> payload
  , makeSawtoothOsc :: MakeSawtoothOsc -> payload
  , makeSinOsc :: MakeSinOsc -> payload
  , makeSpeaker :: MakeSpeaker -> payload
  , makeSquareOsc :: MakeSquareOsc -> payload
  , makeStereoPanner :: MakeStereoPanner -> payload
  , makeTriangleOsc :: MakeTriangleOsc -> payload
  , makeWaveShaper :: MakeWaveShaper -> payload
  , setAnalyserNodeCb :: SetAnalyserNodeCb -> payload
  , setMediaRecorderCb :: SetMediaRecorderCb -> payload
  , setWaveShaperCurve :: SetWaveShaperCurve -> payload
  , setAudioWorkletParameter :: SetAudioWorkletParameter -> payload
  , setBuffer :: SetBuffer -> payload
  , setConvolverBuffer :: SetConvolverBuffer -> payload
  , setPeriodicOsc :: SetPeriodicOsc -> payload
  , setOnOff :: SetOnOff -> payload
  , setBufferOffset :: SetBufferOffset -> payload
  , setDuration :: SetDuration -> payload
  , setLoopStart :: SetLoopStart -> payload
  , setLoopEnd :: SetLoopEnd -> payload
  , setRatio :: SetRatio -> payload
  , setOffset :: SetOffset -> payload
  , setAttack :: SetAttack -> payload
  , setGain :: SetGain -> payload
  , setQ :: SetQ -> payload
  , setPan :: SetPan -> payload
  , setThreshold :: SetThreshold -> payload
  , setRelease :: SetRelease -> payload
  , setKnee :: SetKnee -> payload
  , setDelay :: SetDelay -> payload
  , setPlaybackRate :: SetPlaybackRate -> payload
  , setFrequency :: SetFrequency -> payload
  }

-----

-- TODO:
-- the code below is almost verbatim the same as deku
-- only missing the "send to top" bit
-- merge?
data Stage = Begin | Middle | End

__internalWagsFlatten
  :: forall outputChannels lock payload
   . Maybe String
  -> String
  -> AudioInterpret payload
  -> Audible outputChannels lock payload
  -> Event payload
__internalWagsFlatten
  parent
  pScope
  di@(AudioInterpret { ids, disconnectXFromY })
  (Audible children') = children' # match
  { fixedChannels: \(FixedChannels f) -> oneOfMap node f
  , eventfulNode: \(EventfulNode e) -> keepLatest (map (__internalWagsFlatten parent pScope di) e)
  , node
  , dynamicChannels: \(DynamicChannels children) -> makeEvent \k -> do
      cancelInner <- Ref.new Object.empty
      cancelOuter <-
        -- each child gets its own scope
        subscribe children \inner ->
          do
            -- holds the previous id
            myUnsubId <- ids
            myUnsub <- Ref.new (pure unit)
            eltsUnsubId <- ids
            eltsUnsub <- Ref.new (pure unit)
            myId <- Ref.new DM.Nothing
            myImmediateCancellation <- Ref.new (pure unit)
            myScope <- ids
            stageRef <- Ref.new Begin
            c0 <- subscribe inner \kid' -> do
              stage <- Ref.read stageRef
              case kid', stage of
                Silence, Middle -> do
                  Ref.write End stageRef
                  let
                    mic =
                      ( Ref.read myId >>= traverse_ \old ->
                          for_ parent \p' -> k
                            ( disconnectXFromY
                                { from: old, to: p' }
                            )
                      ) *> join (Ref.read myUnsub)
                        *> join (Ref.read eltsUnsub)
                        *> Ref.modify_
                          (Object.delete myUnsubId)
                          cancelInner
                        *> Ref.modify_
                          (Object.delete eltsUnsubId)
                          cancelInner
                  Ref.write mic myImmediateCancellation *> mic
                Sound (Node kid), Begin -> do
                  -- holds the current id
                  Ref.write Middle stageRef
                  av <- AVar.empty
                  c1 <- subscribe
                    ( kid
                        { parent
                        , scope: myScope
                        , raiseId: \id -> do
                            void $ tryPut id av
                        }
                        di
                    )
                    k
                  cncl <- AVar.take av \q -> case q of
                    Right r -> do
                      Ref.write (DM.Just r) (myId)
                      Ref.modify_ (Object.insert eltsUnsubId c1) cancelInner
                      Ref.write c1 eltsUnsub
                    Left e -> throwException e
                  -- cancel immediately, as it should be run synchronously
                  -- so if this actually does something then we have a problem
                  cncl
                -- ignore
                _,
                _ -> pure unit
            Ref.write c0 myUnsub
            Ref.modify_ (Object.insert myUnsubId c0) cancelInner
            join (Ref.read myImmediateCancellation)
      pure do
        Ref.read cancelInner >>= fold
        cancelOuter
  }
  where
  node (Node e) = e
    { parent
    , scope: pScope
    , raiseId: mempty
    }
    di
