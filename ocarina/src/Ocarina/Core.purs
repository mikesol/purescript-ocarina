module Ocarina.Core
  ( Allpass(..)
  , Analyser(..)
  , Audible
  , AudibleChild
  , AudioCancel'
  , AudioCancel(..)
  , AudioEnvelope'
  , AudioEnvelope(..)
  , AudioInterpret(..)
  , AudioNumeric'
  , AudioNumeric(..)
  , AudioOnOff(..)
  , AudioParameter(..)
  , AudioSudden'
  , AudioSudden(..)
  , AudioUnit'
  , AudioUnit(..)
  , AudioWorkletNode(..)
  , AudioWorkletNodeOptions_(..)
  , Bandpass(..)
  , ChannelCountMode(..)
  , ChannelInterpretation(..)
  , ConnectXToY
  , ConnectXToY_
  , Constant(..)
  , Delay(..)
  , DeleteFromCache
  , DisconnectXFromY
  , DisconnectXFromY_
  , DynamicsCompressor(..)
  , FFIAudioParameter(..)
  , FFIAudioUnit'
  , FFIAudioUnit(..)
  , Gain(..)
  , Highpass(..)
  , Highshelf(..)
  , InitialAudioParameter
  , InitializeAllpass(..)
  , InitializeAnalyser(..)
  , InitializeAudioWorkletNode(..)
  , InitializeBandpass(..)
  , InitializeConstant(..)
  , InitializeConvolver(..)
  , InitializeDelay(..)
  , InitializeDynamicsCompressor(..)
  , InitializeGain(..)
  , InitializeHighpass(..)
  , InitializeHighshelf(..)
  , InitializeIIRFilter(..)
  , InitializeLoopBuf(..)
  , InitializeLowpass(..)
  , InitializeLowshelf(..)
  , InitializeMediaElement(..)
  , InitializeMicrophone(..)
  , InitializeNotch(..)
  , InitializePeaking(..)
  , InitializePeriodicOsc(..)
  , InitializePlayBuf(..)
  , InitializeRecorder(..)
  , InitializeSawtoothOsc(..)
  , InitializeSinOsc(..)
  , InitializeSquareOsc(..)
  , InitializeStereoPanner(..)
  , InitializeTriangleOsc(..)
  , InitializeWaveShaper(..)
  , LoopBuf(..)
  , Lowpass(..)
  , Lowshelf(..)
  , MakeAllpass
  , MakeAllpass_
  , MakeAnalyser
  , MakeAudioWorkletNode
  , MakeAudioWorkletNode_
  , MakeBandpass
  , MakeBandpass_
  , MakeConstant
  , MakeConstant_
  , MakeConvolver
  , MakeDelay
  , MakeDelay_
  , MakeDynamicsCompressor
  , MakeDynamicsCompressor_
  , MakeGain
  , MakeGain_
  , MakeHighpass
  , MakeHighpass_
  , MakeHighshelf
  , MakeHighshelf_
  , MakeIIRFilter
  , MakeLoopBuf
  , MakeLoopBuf_
  , MakeLowpass
  , MakeLowpass_
  , MakeLowshelf
  , MakeLowshelf_
  , MakeMediaElement
  , MakeMicrophone
  , MakeNotch
  , MakeNotch_
  , MakePeaking
  , MakePeaking_
  , MakePeriodicOsc
  , MakePeriodicOsc_
  , MakePlayBuf
  , MakePlayBuf_
  , MakeRecorder
  , MakeSawtoothOsc
  , MakeSawtoothOsc_
  , MakeSinOsc
  , MakeSinOsc_
  , MakeSpeaker
  , MakeSquareOsc
  , MakeSquareOsc_
  , MakeStereoPanner
  , MakeStereoPanner_
  , MakeTriangleOsc
  , MakeTriangleOsc_
  , MakeWaveShaper
  , Node(..)
  , NodeC'
  , Notch(..)
  , OnOff(..)
  , Oversample(..)
  , Peaking(..)
  , PeriodicOsc(..)
  , PeriodicOscSpec(..)
  , PlayBuf(..)
  , Po2(..)
  , RealImg(..)
  , SawtoothOsc(..)
  , SetAnalyserNodeCb
  , SetAttack
  , SetAudioWorkletParameter
  , SetBuffer
  , SetBufferOffset
  , SetConvolverBuffer
  , SetDelay
  , SetDuration
  , SetFrequency
  , SetGain
  , SetKnee
  , SetLoopEnd
  , SetLoopStart
  , SetMediaRecorderCb
  , SetOffset
  , SetOnOff
  , SetPan
  , SetPeriodicOsc
  , SetPlaybackRate
  , SetQ
  , SetRatio
  , SetRelease
  , SetThreshold
  , SetWaveShaperCurve
  , SinOsc(..)
  , SquareOsc(..)
  , StereoPanner(..)
  , Transition(..)
  , TriangleOsc(..)
  , _cancel
  , _cancel'
  , _envelope
  , _envelope'
  , _exponential
  , _fourX
  , _linear
  , _step
  , _none
  , _numeric
  , _numeric'
  , _off
  , _on
  , _realImg
  , _sudden
  , _sudden'
  , _twoX
  , _unit
  , _unit'
  , _wave
  , apOff
  , apOn
  , bangOn
  , c1
  , class OpticN
  , class ToAudioOnOff
  , class ToAudioParameter
  , dt
  , module Bolson.Core
  , opticN
  , silence
  , toAudioOnOff
  , toAudioParameter
  )
  where

import Prelude

import Bolson.Core (dyn, fixed)
import Bolson.Core as Bolson
import Control.Monad.ST (ST)
import Control.Monad.ST.Global as Region
import Data.FastVect.FastVect (Vect)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lens (Optic', over)
import Data.Lens.Iso.Newtype (_Newtype, unto)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong (class Strong)
import Data.Show.Generic (genericShow)
import Data.Typelevel.Num (D1)
import Data.Variant (Variant, inj, match)
import FRP.Poll (Poll)
import Foreign (Foreign)
import Foreign.Object (Object)
import Ocarina.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)
import Prim.Row (class Cons)
import Simple.JSON as JSON
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- start param
newtype Transition = Transition
  (Variant (linear :: Unit, exponential :: Unit, step :: Unit))

_linear :: Transition
_linear = Transition $ inj (Proxy :: _ "linear") unit

_exponential :: Transition
_exponential = Transition $ inj (Proxy :: _ "exponential") unit

_step :: Transition
_step = Transition $ inj (Proxy :: _ "step") unit

_numeric' :: forall payload. AudioNumeric' -> AudioParameter payload
_numeric' = _numeric <<< AudioNumeric

_numeric :: forall payload. AudioNumeric -> AudioParameter payload
_numeric = AudioParameter <<< inj (Proxy :: _ "numeric")

_unit' :: forall payload. AudioUnit' payload -> AudioParameter payload
_unit' = _unit <<< AudioUnit

_unit :: forall payload. AudioUnit payload -> AudioParameter payload
_unit = AudioParameter <<< inj (Proxy :: _ "unit")

_envelope :: forall payload. AudioEnvelope -> AudioParameter payload
_envelope = AudioParameter <<< inj (Proxy :: _ "envelope")

_envelope' :: forall payload. AudioEnvelope' -> AudioParameter payload
_envelope' = _envelope <<< AudioEnvelope

_cancel :: forall payload. AudioCancel -> AudioParameter payload
_cancel = AudioParameter <<< inj (Proxy :: _ "cancel")

_cancel' :: forall payload. AudioCancel' -> AudioParameter payload
_cancel' = _cancel <<< AudioCancel

_sudden :: forall payload. AudioSudden -> AudioParameter payload
_sudden = AudioParameter <<< inj (Proxy :: _ "sudden")

_sudden' :: forall payload. AudioSudden' -> AudioParameter payload
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

type AudioUnit' payload = { u :: Audible D1 payload }
newtype AudioUnit payload = AudioUnit (AudioUnit' payload)

type AudioSudden' = { n :: Number }
newtype AudioSudden = AudioSudden AudioSudden'

derive instance Newtype AudioSudden _

type InitialAudioParameter = Number

newtype AudioParameter payload = AudioParameter
  ( Variant
      ( numeric :: AudioNumeric
      , envelope :: AudioEnvelope
      , cancel :: AudioCancel
      , sudden :: AudioSudden
      , unit :: AudioUnit payload
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
  => Poll nt
bangOn = pure (wrap $ inj (Proxy :: _ "onOff") apOn)

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

class ToAudioParameter i payload where
  toAudioParameter :: i -> AudioParameter payload

instance ToAudioParameter Number payload where
  toAudioParameter n = _sudden (AudioSudden { n })

instance (TypeEquals p0 p1) => ToAudioParameter (AudioParameter p0) p1 where
  toAudioParameter = (unsafeCoerce :: AudioParameter p0 -> AudioParameter p1)

instance ToAudioParameter AudioNumeric p where
  toAudioParameter = _numeric

instance ToAudioParameter AudioSudden p where
  toAudioParameter = _sudden

instance ToAudioParameter AudioCancel p where
  toAudioParameter = _cancel

instance ToAudioParameter AudioEnvelope p where
  toAudioParameter = _envelope

instance ( TypeEquals p0 p1) => ToAudioParameter (AudioUnit p0) p1 where
  toAudioParameter = _unit <<< (unsafeCoerce :: AudioUnit p0 -> AudioUnit p1)

instance ( TypeEquals p0 p1) => ToAudioParameter (Audible D1 p0) p1 where
  toAudioParameter = toAudioParameter <<< AudioUnit <<< { u: _ }

c1 :: forall p. (forall o. Audible o p) -> Audible D1 p
c1 = unsafeCoerce

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

type NodeC' payload =
  Bolson.PSR ()
  -> AudioInterpret payload
  -> Poll payload

newtype Node :: forall k. k -> Type -> Type
newtype Node outputChannels payload = Node (NodeC' payload)

silence :: AudibleChild
silence = Bolson.Remove

type Audible :: forall k. k -> Type -> Type
type Audible outputChannels payload = Bolson.Entity Void (Node outputChannels payload)
type AudibleChild = Bolson.Child Void

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

derive instance newtypeAllpass :: Newtype (Allpass parameter) _
newtype Allpass parameter = Allpass
  ( Variant
      ( frequency :: AudioParameter parameter
      , q :: AudioParameter parameter
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
  , scope :: Maybe String
  , frequency :: param
  , q :: param
  }

type MakeAllpass = MakeAllpass_ InitialAudioParameter
type MakeAllpass' payload = MakeAllpass_ (AudioParameter payload)

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
  , scope :: Maybe String
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
  , scope :: Maybe String
  , options :: AudioWorkletNodeOptions_ param
  }

type MakeAudioWorkletNode = MakeAudioWorkletNode_ InitialAudioParameter
type MakeAudioWorkletNode' payload = MakeAudioWorkletNode_ (AudioParameter payload)

derive instance newtypeBandpass :: Newtype (Bandpass payload) _
newtype Bandpass payload = Bandpass
  ( Variant
      ( frequency :: AudioParameter payload
      , q :: AudioParameter payload
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
  , scope :: Maybe String
  , frequency :: param
  , q :: param
  }

type MakeBandpass = MakeBandpass_ InitialAudioParameter
type MakeBandpass' payload = MakeBandpass_ (AudioParameter payload)

derive instance newtypeConstant :: Newtype (Constant payload) _
newtype Constant payload = Constant
  (Variant (offset :: AudioParameter payload, onOff :: AudioOnOff))

derive instance newtypeInitializeConstant :: Newtype InitializeConstant _
newtype InitializeConstant = InitializeConstant
  { offset :: InitialAudioParameter }

type MakeConstant_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , offset :: param
  )

type MakeConstant = { | MakeConstant_ InitialAudioParameter }
type MakeConstant' payload = { onOff :: AudioOnOff | MakeConstant_ (AudioParameter payload) }

derive instance newtypeInitializeConvolver :: Newtype InitializeConvolver _
newtype InitializeConvolver = InitializeConvolver
  { buffer :: BrowserAudioBuffer }

type MakeConvolver =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , buffer :: BrowserAudioBuffer
  }

derive instance newtypeDelay :: Newtype (Delay payload) _
newtype Delay payload = Delay (Variant (delayTime :: AudioParameter payload))

derive instance newtypeInitializeDelay :: Newtype InitializeDelay _
newtype InitializeDelay = InitializeDelay { delayTime :: InitialAudioParameter, maxDelayTime :: InitialAudioParameter }
type MakeDelay_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , delayTime :: param
  , maxDelayTime :: Number
  }

type MakeDelay = MakeDelay_ InitialAudioParameter
type MakeDelay' payload = MakeDelay_ (AudioParameter payload)

derive instance newtypeDynamicsCompressor :: Newtype (DynamicsCompressor payload) _
newtype DynamicsCompressor payload = DynamicsCompressor
  ( Variant
      ( threshold :: AudioParameter payload
      , knee :: AudioParameter payload
      , ratio :: AudioParameter payload
      , attack :: AudioParameter payload
      , release :: AudioParameter payload
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
  , scope :: Maybe String
  , threshold :: param
  , knee :: param
  , ratio :: param
  , attack :: param
  , release :: param
  }

type MakeDynamicsCompressor = MakeDynamicsCompressor_ InitialAudioParameter
type MakeDynamicsCompressor' payload = MakeDynamicsCompressor_ (AudioParameter payload)

derive instance newtypeGain :: Newtype (Gain payload) _
newtype Gain payload = Gain (Variant (gain :: AudioParameter payload))

derive instance newtypeInitializeGain :: Newtype InitializeGain _
newtype InitializeGain = InitializeGain { gain :: InitialAudioParameter }
type MakeGain_ param =
  { id :: String, parent :: Maybe String, scope :: Maybe String, gain :: param }

type MakeGain = MakeGain_ InitialAudioParameter
type MakeGain' payload = MakeGain_ (AudioParameter payload)

derive instance newtypeHighpass :: Newtype (Highpass payload) _
newtype Highpass payload = Highpass
  ( Variant
      ( frequency :: AudioParameter payload
      , q :: AudioParameter payload
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
  , scope :: Maybe String
  , frequency :: param
  , q :: param
  }

type MakeHighpass = MakeHighpass_ InitialAudioParameter
type MakeHighpass' payload = MakeHighpass_ (AudioParameter payload)

derive instance newtypeHighshelf :: Newtype (Highshelf payload) _
newtype Highshelf payload = Highshelf
  ( Variant
      ( frequency :: AudioParameter payload
      , gain :: AudioParameter payload
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
  , scope :: Maybe String
  , frequency :: param
  , gain :: param
  }

type MakeHighshelf = MakeHighshelf_ InitialAudioParameter
type MakeHighshelf' payload = MakeHighshelf_ (AudioParameter payload)

derive instance newtypeInitializeIIRFilter :: Newtype (InitializeIIRFilter feedforward feedback) _
newtype InitializeIIRFilter (feedforward :: Int) (feedback :: Int) = InitializeIIRFilter
  { feedforward :: Vect feedforward Number, feedback :: Vect feedback Number }

type MakeIIRFilter =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , feedforward :: Array Number
  , feedback :: Array Number
  }

derive instance newtypeLoopBuf :: Newtype (LoopBuf payload) _
newtype LoopBuf payload = LoopBuf
  ( Variant
      ( buffer :: BrowserAudioBuffer
      , onOff :: AudioOnOff
      , playbackRate :: AudioParameter payload
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
  , scope :: Maybe String
  , buffer :: BrowserAudioBuffer
  , playbackRate :: param
  , loopStart :: Number
  , loopEnd :: Number
  , duration :: Maybe Number
  )

type MakeLoopBuf = { | MakeLoopBuf_ InitialAudioParameter }
type MakeLoopBuf' payload = { onOff :: AudioOnOff | MakeLoopBuf_ (AudioParameter payload) }

derive instance newtypeLowpass :: Newtype (Lowpass payload) _
newtype Lowpass payload = Lowpass
  ( Variant
      ( frequency :: AudioParameter payload
      , q :: AudioParameter payload
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
  , scope :: Maybe String
  , frequency :: param
  , q :: param
  }

type MakeLowpass = MakeLowpass_ InitialAudioParameter
type MakeLowpass' payload = MakeLowpass_ (AudioParameter payload)

derive instance newtypeLowshelf :: Newtype (Lowshelf payload) _
newtype Lowshelf payload = Lowshelf
  ( Variant
      ( frequency :: AudioParameter payload
      , gain :: AudioParameter payload
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
  , scope :: Maybe String
  , frequency :: param
  , gain :: param
  }

type MakeLowshelf = MakeLowshelf_ InitialAudioParameter
type MakeLowshelf' payload = MakeLowshelf_ (AudioParameter payload)

derive instance newtypeInitializeMediaElement ::
  Newtype InitializeMediaElement _

newtype InitializeMediaElement = InitializeMediaElement
  { element :: BrowserMediaElement
  }

type MakeMediaElement =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
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
  , scope :: Maybe String
  }

derive instance newtypeNotch :: Newtype (Notch payload) _
newtype Notch payload = Notch
  ( Variant
      ( frequency :: AudioParameter payload
      , q :: AudioParameter payload
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
  , scope :: Maybe String
  , frequency :: param
  , q :: param
  }

type MakeNotch = MakeNotch_ InitialAudioParameter
type MakeNotch' payload = MakeNotch_ (AudioParameter payload)

derive instance newtypePeaking :: Newtype (Peaking payload) _
newtype Peaking payload = Peaking
  ( Variant
      ( frequency :: AudioParameter payload
      , q :: AudioParameter payload
      , gain :: AudioParameter payload
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
  , scope :: Maybe String
  , frequency :: param
  , q :: param
  , gain :: param
  }

type MakePeaking = MakePeaking_ InitialAudioParameter
type MakePeaking' payload = MakePeaking_ (AudioParameter payload)

derive instance newtypePeriodicOsc :: Newtype (PeriodicOsc payload) _
newtype PeriodicOsc payload =
  PeriodicOsc
    ( Variant
        ( spec :: PeriodicOscSpec
        , onOff :: AudioOnOff
        , frequency :: AudioParameter payload
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
  , scope :: Maybe String
  , spec :: PeriodicOscSpec
  , frequency :: param
  )

type MakePeriodicOsc = { | MakePeriodicOsc_ InitialAudioParameter }
type MakePeriodicOsc' payload =
  { onOff :: AudioOnOff | MakePeriodicOsc_ (AudioParameter payload) }

derive instance newtypePlayBuf :: Newtype (PlayBuf payload) _
newtype PlayBuf payload =
  PlayBuf
    ( Variant
        ( buffer :: BrowserAudioBuffer
        , bufferOffset :: Number
        , onOff :: AudioOnOff
        , playbackRate :: AudioParameter payload
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
  , scope :: Maybe String
  , buffer :: BrowserAudioBuffer
  , bufferOffset :: Number
  , playbackRate :: param
  , duration :: Maybe Number
  )

type MakePlayBuf = { | MakePlayBuf_ InitialAudioParameter }
type MakePlayBuf' payload = { onOff :: AudioOnOff | MakePlayBuf_ (AudioParameter payload) }

derive instance newtypeInitializeRecorder :: Newtype InitializeRecorder _
newtype InitializeRecorder = InitializeRecorder { cb :: MediaRecorderCb }
type MakeRecorder =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , cb :: MediaRecorderCb
  }

type MakeSpeaker = { id :: String }

derive instance newtypeSawtoothOsc :: Newtype (SawtoothOsc payload) _
newtype SawtoothOsc payload =
  SawtoothOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter payload
        )
    )

derive instance newtypeInitializeSawtoothOsc :: Newtype InitializeSawtoothOsc _
newtype InitializeSawtoothOsc = InitializeSawtoothOsc
  { frequency :: InitialAudioParameter
  }

type MakeSawtoothOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , frequency :: param
  )

type MakeSawtoothOsc = { | MakeSawtoothOsc_ InitialAudioParameter }
type MakeSawtoothOsc' payload =
  { onOff :: AudioOnOff | MakeSawtoothOsc_ (AudioParameter payload) }

derive instance newtypeSinOsc :: Newtype (SinOsc payload) _
newtype SinOsc payload = SinOsc
  (Variant (frequency :: AudioParameter payload, onOff :: AudioOnOff))

derive instance newtypeInitializeSinOsc :: Newtype InitializeSinOsc _
newtype InitializeSinOsc = InitializeSinOsc
  { frequency :: InitialAudioParameter
  }

type MakeSinOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , frequency :: param
  )

type MakeSinOsc = { | MakeSinOsc_ InitialAudioParameter }
type MakeSinOsc' payload = { onOff :: AudioOnOff | MakeSinOsc_ (AudioParameter payload) }

derive instance newtypeSquareOsc :: Newtype (SquareOsc payload) _
newtype SquareOsc payload =
  SquareOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter payload
        )
    )

derive instance newtypeInitializeSquareOsc :: Newtype InitializeSquareOsc _
newtype InitializeSquareOsc = InitializeSquareOsc
  { frequency :: InitialAudioParameter
  }

type MakeSquareOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , frequency :: param
  )

type MakeSquareOsc = { | MakeSquareOsc_ InitialAudioParameter }
type MakeSquareOsc' payload = { onOff :: AudioOnOff | MakeSquareOsc_ (AudioParameter payload) }

derive instance newtypeStereoPanner :: Newtype (StereoPanner payload) _
newtype StereoPanner payload =
  StereoPanner (Variant (pan :: AudioParameter payload))

derive instance newtypeInitializeStereoPanner ::
  Newtype InitializeStereoPanner _

newtype InitializeStereoPanner = InitializeStereoPanner
  { pan :: InitialAudioParameter }

type MakeStereoPanner_ param =
  { id :: String, parent :: Maybe String, scope :: Maybe String, pan :: param }

type MakeStereoPanner = MakeStereoPanner_ InitialAudioParameter
type MakeStereoPanner' payload = MakeStereoPanner_ (AudioParameter payload)

derive instance newtypeTriangleOsc :: Newtype (TriangleOsc parameter) _
newtype TriangleOsc parameter =
  TriangleOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter parameter
        )
    )

derive instance newtypeInitializeTriangleOsc :: Newtype InitializeTriangleOsc _
newtype InitializeTriangleOsc = InitializeTriangleOsc
  { frequency :: InitialAudioParameter
  }

type MakeTriangleOsc_ param =
  ( id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , frequency :: param
  )

type MakeTriangleOsc = { | MakeTriangleOsc_ InitialAudioParameter }
type MakeTriangleOsc' payload =
  { onOff :: AudioOnOff | MakeTriangleOsc_ (AudioParameter payload) }

derive instance newtypeInitializeWaveShaper :: Newtype InitializeWaveShaper _
newtype InitializeWaveShaper = InitializeWaveShaper
  { curve :: BrowserFloatArray
  , oversample :: Oversample
  }

type MakeWaveShaper =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
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
  { ids :: ST Region.Global Int
  , deferPayload :: List Int -> payload -> payload
  , forcePayload :: List Int -> payload
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

derive instance Newtype (AudioInterpret payload) _