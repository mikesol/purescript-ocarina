module Deku.Core where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant, inj, match)
import Data.Variant.Maybe (Maybe)
import Deku.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)
import FRP.Behavior (ABehavior)
import FRP.Event.Pure (PureEvent)
import Foreign (Foreign)
import Foreign.Object (Object)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))

--
type AudioWorkletNodeOptions_' =
  { name :: String
  , numberOfInputs :: Int
  , numberOfOutputs :: Int
  , outputChannelCount :: Array Int
  , parameterData :: Object InitialAudioParameter
  , processorOptions :: Foreign
  }

type AudioWorkletNodeOptions_S' =
  { name :: String
  , numberOfInputs :: Int
  , numberOfOutputs :: Int
  , outputChannelCount :: Array Int
  , parameterData :: Object InitialAudioParameter
  , processorOptions :: String
  }

audioWorkletNodeOptionsForInstances
  :: AudioWorkletNodeOptions_' -> AudioWorkletNodeOptions_S'
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

newtype AudioWorkletNodeOptions_ = AudioWorkletNodeOptions_
  { name :: String
  , numberOfInputs :: Int
  , numberOfOutputs :: Int
  , outputChannelCount :: Array Int
  , parameterData :: Object InitialAudioParameter
  , processorOptions :: Foreign
  }

derive instance newtypeAudioWorkletNodeOptions_ ::
  Newtype AudioWorkletNodeOptions_ _

instance eqAudioWorkletNodeOptions_ :: Eq AudioWorkletNodeOptions_ where
  eq = eq `on` (unwrap >>> audioWorkletNodeOptionsForInstances)

instance ordAudioWorkletNodeOptions_ :: Ord AudioWorkletNodeOptions_ where
  compare = compare `on` (unwrap >>> audioWorkletNodeOptionsForInstances)

instance showAudioWorkletNodeOptions_ :: Show AudioWorkletNodeOptions_ where
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

newtype Input = Input String

newtype Node outputChannels produced consumed event payload = Node
  (String -> AudioInterpret event payload -> event payload)

newtype GainInput outputChannels produced consumed event payload = GainInput
  (NonEmptyArray (Node outputChannels produced consumed event payload))

newtype Subgraph index env outputChannels event payload =
  Subgraph
    ( forall (produced :: Row Type) (consumed :: Row Type)
       . index
      -> event env
      -> Node outputChannels produced consumed event payload
    )

newtype Transition = Transition
  (Variant (linear :: Unit, exponential :: Unit, step :: Unit))

derive instance eqTransition :: Eq Transition
derive instance ordTransition :: Ord Transition
derive instance newtypeTransition :: Newtype Transition _
derive newtype instance showTransition :: Show Transition

_number :: AudioNumeric -> AudioParameter
_number = AudioParameter <<< inj (Proxy :: _ "numeric")
_envelope :: AudioEnvelope -> AudioParameter
_envelope = AudioParameter <<< inj (Proxy :: _ "envelope")
_cancel :: AudioCancel -> AudioParameter
_cancel = AudioParameter <<< inj (Proxy :: _ "cancel")
_sudden :: AudioSudden -> AudioParameter
_sudden = AudioParameter <<< inj (Proxy :: _ "sudden")

type AudioNumeric = { n :: Number, o :: Number, t :: Transition }
type AudioEnvelope = { p :: Array Number, o :: Number, d :: Number }
type AudioCancel = { o :: Number }
type AudioSudden = { n :: Number }

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
  { onOff :: OnOff
  , timeOffset :: Number
  }

derive instance eqAudioOnOff :: Eq AudioOnOff
derive instance ordAudioOnOff :: Ord AudioOnOff
derive instance newtypeAudioOnOff :: Newtype AudioOnOff _
derive instance genericAudioOnOff :: Generic AudioOnOff _

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

type ConnectXToY = { from :: String, to :: String }
type DisconnectXFromY = { from :: String, to :: String }
type DestroyUnit = { id :: String, unit :: String }
newtype Allpass = Allpass
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
      )
  )
type InitializeAllpass =
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
type MakeAllpass =
  { id :: String
  , parent :: String
  , frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
newtype Analyser = Analyser (Variant (cb :: AnalyserNodeCb))
type InitializeAnalyser = { cb :: AnalyserNodeCb }
type MakeAnalyser = { id :: String, parent :: String, cb :: AnalyserNodeCb }
newtype AudioWorkletNode parameterData = AudioWorkletNode
  (Variant parameterData)
type InitializeAudioWorkletNode
  (name :: Symbol)
  numberOfInputs
  numberOfOutputs
  outputChannelCount
  parameterData
  processorOptions =
  { name :: Proxy name
  , numberOfInputs :: numberOfInputs
  , numberOfOutputs :: numberOfOutputs
  , outputChannelCount :: outputChannelCount
  , parameterData :: { | parameterData }
  , processorOptions :: { | processorOptions }
  }
type MakeAudioWorkletNode =
  { id :: String, parent :: String, options :: AudioWorkletNodeOptions_ }
newtype Bandpass = Bandpass
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
      )
  )
type InitializeBandpass =
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
type MakeBandpass =
  { id :: String
  , parent :: String
  , frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
newtype Constant = Constant
  (Variant (offset :: AudioParameter, onOff :: AudioOnOff))
type InitializeConstant = { offset :: InitialAudioParameter, onOff :: AudioOnOff }
type MakeConstant =
  { id :: String
  , parent :: String
  , offset :: InitialAudioParameter
  , onOff :: AudioOnOff
  }
type InitializeConvolver = { buffer :: BrowserAudioBuffer }
type MakeConvolver =
  { id :: String, parent :: String, buffer :: BrowserAudioBuffer }
newtype Delay = Delay (Variant (delayTime :: AudioParameter))
type InitializeDelay = { delayTime :: InitialAudioParameter }
type MakeDelay = { id :: String, parent :: String, delayTime :: InitialAudioParameter }
newtype DynamicsCompressor = DynamicsCompressor
  ( Variant
      ( threshold :: AudioParameter
      , knee :: AudioParameter
      , ratio :: AudioParameter
      , attack :: AudioParameter
      , release :: AudioParameter
      )
  )
type InitializeDynamicsCompressor =
  { threshold :: InitialAudioParameter
  , knee :: InitialAudioParameter
  , ratio :: InitialAudioParameter
  , attack :: InitialAudioParameter
  , release :: InitialAudioParameter
  }
type MakeDynamicsCompressor =
  { id :: String
  , parent :: String
  , threshold :: InitialAudioParameter
  , knee :: InitialAudioParameter
  , ratio :: InitialAudioParameter
  , attack :: InitialAudioParameter
  , release :: InitialAudioParameter
  }
newtype Gain = Gain (Variant (gain :: AudioParameter))
type InitializeGain = { gain :: InitialAudioParameter }
type MakeGain = { id :: String, parent :: String, gain :: InitialAudioParameter }
newtype Highpass = Highpass
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
      )
  )
type InitializeHighpass =
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
type MakeHighpass =
  { id :: String
  , parent :: String
  , frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
newtype Highshelf = Highshelf
  ( Variant
      ( frequency :: AudioParameter
      , gain :: AudioParameter
      )
  )
type InitializeHighshelf =
  { frequency :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }
type MakeHighshelf =
  { id :: String
  , parent :: String
  , frequency :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }
type MakeInput = { id :: String, parent :: String }
newtype LoopBuf = LoopBuf
  ( Variant
      ( buffer :: BrowserAudioBuffer
      , onOff :: AudioOnOff
      , playbackRate :: AudioParameter
      , loopStart :: Number
      , loopEnd :: Number
      )
  )
type InitializeLoopBuf =
  { buffer :: BrowserAudioBuffer
  , onOff :: AudioOnOff
  , playbackRate :: InitialAudioParameter
  , loopStart :: Number
  , loopEnd :: Number
  -- duration in initializer but not setter for now
  , duration :: Maybe Number
  }
type MakeLoopBuf =
  { id :: String
  , parent :: String
  , buffer :: BrowserAudioBuffer
  , onOff :: AudioOnOff
  , playbackRate :: InitialAudioParameter
  , loopStart :: Number
  , loopEnd :: Number
  , duration :: Maybe Number
  }
newtype Lowpass = Lowpass
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
      )
  )
type InitializeLowpass =
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
type MakeLowpass =
  { id :: String
  , parent :: String
  , frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
newtype Lowshelf = Lowshelf
  ( Variant
      ( frequency :: AudioParameter
      , gain :: AudioParameter
      )
  )
type InitializeLowshelf =
  { frequency :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }
type MakeLowshelf =
  { id :: String
  , parent :: String
  , frequency :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }
type InitializeMediaElement =
  { element :: BrowserMediaElement
  }
type MakeMediaElement =
  { id :: String
  , parent :: String
  , element :: BrowserMediaElement
  }
type InitializeMicrophone =
  { microphone :: BrowserMicrophone, parent :: String }
type MakeMicrophone =
  { id :: String, microphone :: BrowserMicrophone, parent :: String }
newtype Notch = Notch
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
      )
  )
type InitializeNotch =
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
type MakeNotch =
  { id :: String
  , parent :: String
  , frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  }
newtype Peaking = Peaking
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
      , gain :: AudioParameter
      )
  )
type InitializePeaking =
  { frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }
type MakePeaking =
  { id :: String
  , parent :: String
  , frequency :: InitialAudioParameter
  , q :: InitialAudioParameter
  , gain :: InitialAudioParameter
  }
newtype PeriodicOsc =
  PeriodicOsc
    ( Variant
        ( spec :: PeriodicOscSpec
        , onOff :: AudioOnOff
        , frequency :: AudioParameter
        )
    )
type InitializePeriodicOsc =
  { spec :: PeriodicOscSpec
  , onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
type MakePeriodicOsc =
  { id :: String
  , parent :: String
  , spec :: PeriodicOscSpec
  , onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
newtype PlayBuf =
  PlayBuf
    ( Variant
        ( buffer :: BrowserAudioBuffer
        , bufferOffset :: Number
        , onOff :: AudioOnOff
        , playbackRate :: AudioParameter
        )
    )
type InitializePlayBuf =
  { buffer :: BrowserAudioBuffer
  , bufferOffset :: Number
  , onOff :: AudioOnOff
  , playbackRate :: InitialAudioParameter
  , duration :: Maybe Number
  }
type MakePlayBuf =
  { id :: String
  , parent :: String
  , buffer :: BrowserAudioBuffer
  , bufferOffset :: Number
  , onOff :: AudioOnOff
  , playbackRate :: InitialAudioParameter
  , duration :: Maybe Number
  }
type InitializeRecorder = { cb :: MediaRecorderCb }
type MakeRecorder = { id :: String, parent :: String, cb :: MediaRecorderCb }
type MakeSpeaker = { id :: String }
newtype SawtoothOsc =
  SawtoothOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter
        )
    )
type InitializeSawtoothOsc =
  { onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
type MakeSawtoothOsc =
  { id :: String
  , parent :: String
  , onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
newtype SinOsc = SinOsc
  (Variant (frequency :: AudioParameter, onOff :: AudioOnOff))
type InitializeSinOsc =
  { onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
type MakeSinOsc =
  { id :: String
  , parent :: String
  , onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
newtype SquareOsc =
  SquareOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter
        )
    )
type InitializeSquareOsc =
  { onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
type MakeSquareOsc =
  { id :: String
  , parent :: String
  , onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
newtype StereoPanner =
  StereoPanner (Variant (pan :: AudioParameter))
type InitializeStereoPanner =
  { pan :: InitialAudioParameter }
type MakeStereoPanner =
  { id :: String, parent :: String, pan :: InitialAudioParameter }
newtype TriangleOsc =
  TriangleOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter
        )
    )
type InitializeTriangleOsc =
  { onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
type MakeTriangleOsc =
  { id :: String
  , parent :: String
  , onOff :: AudioOnOff
  , frequency :: InitialAudioParameter
  }
type InitializeWaveshaper =
  { curve :: BrowserFloatArray
  , oversample :: Oversample
  }
type MakeWaveShaper =
  { id :: String
  , parent :: String
  , curve :: BrowserFloatArray
  , oversample :: Oversample
  }
newtype Tumult =
  Tumuilt (Variant (instructions :: forall r. PureEvent r Unit -> PureEvent r Instruction))
type InitializeTumult =
  { instructions :: forall r. PureEvent r Unit -> PureEvent r Instruction
  }
type MakeTumult =
  { id :: String
  , parent :: String
  , instructions :: forall r. PureEvent r Unit -> PureEvent r Instruction
  }
type MakeTumultInternal =
  { id :: String
  , parent :: String
  , instructions :: Set Instruction
  }
type MakeSubgraph
  index
  env
  (outputChannels :: Type)
  event
  payload =
  { id :: String
  , parent :: String
  , scenes :: Subgraph index env outputChannels event payload
  }
type InsertOrUpdateSubgraph index env =
  { id :: String
  , index :: index
  , env :: env
  , pos :: Int
  }
type RemoveSubgraph index =
  { id :: String
  , index :: index
  , pos :: Int
  }
type SetAnalyserNodeCb = { id :: String, cb :: AnalyserNodeCb }
type SetMediaRecorderCb = { id :: String, cb :: MediaRecorderCb }
type SetAudioWorkletParameter =
  { id :: String, paramName :: String, paramValue :: AudioParameter }
type SetBuffer = { id :: String, buffer :: BrowserAudioBuffer }
type SetConvolverBuffer = { id :: String, buffer :: BrowserAudioBuffer }
type SetPeriodicOsc = { id :: String, spec :: PeriodicOscSpec }
type SetOnOff = { id :: String, onOff :: AudioOnOff }
type SetBufferOffset = { id :: String, bufferOffset :: Number }
type SetLoopStart = { id :: String, loopStart :: Number }
type SetLoopEnd = { id :: String, loopEnd :: Number }
type SetRatio = { id :: String, ratio :: AudioParameter }
type SetOffset = { id :: String, offset :: AudioParameter }
type SetAttack = { id :: String, attack :: AudioParameter }
type SetGain = { id :: String, gain :: AudioParameter }
type SetQ = { id :: String, q :: AudioParameter }
type SetPan = { id :: String, pan :: AudioParameter }
type SetThreshold = { id :: String, threshold :: AudioParameter }
type SetRelease = { id :: String, release :: AudioParameter }
type SetKnee = { id :: String, knee :: AudioParameter }
type SetDelay = { id :: String, delayTime :: AudioParameter }
type SetPlaybackRate = { id :: String, playbackRate :: AudioParameter }
type SetFrequency = { id :: String, frequency :: AudioParameter }
type SetWaveShaperCurve = { id :: String, curve :: BrowserFloatArray }
type SetTumult =
  { id :: String
  , terminus :: String
  , instructions :: forall r. PureEvent r Unit -> PureEvent r Instruction
  }
type SetTumultInternal =
  { id :: String
  , terminus :: String
  , instructions :: Set Instruction
  }
newtype AudioInterpret event payload = AudioInterpret
  { ids :: ABehavior event String
  , destroyUnit :: DestroyUnit -> payload
  , disconnectXFromY :: DisconnectXFromY -> payload
  , connectXToY :: ConnectXToY -> payload
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
  , makeInput :: MakeInput -> payload
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
  , makeSubgraph ::
      forall index env outputChannels
       . MakeSubgraph index env outputChannels event payload
      -> payload
  , makeTriangleOsc :: MakeTriangleOsc -> payload
  , makeTumult :: MakeTumultInternal -> payload
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
  , removeSubgraph ::
      forall index
       . RemoveSubgraph index
      -> payload
  , insertOrUpdateSubgraph ::
      forall index env
       . InsertOrUpdateSubgraph index env
      -> payload
  , setTumult :: SetTumultInternal -> payload
  }

type Instruction' =
  ( makeAllpass :: MakeAllpass
  , makeAnalyser :: MakeAnalyser
  , makeAudioWorkletNode :: MakeAudioWorkletNode
  , makeBandpass :: MakeBandpass
  , makeConstant :: MakeConstant
  , makeConvolver :: MakeConvolver
  , makeDelay :: MakeDelay
  , makeDynamicsCompressor :: MakeDynamicsCompressor
  , makeGain :: MakeGain
  , makeHighpass :: MakeHighpass
  , makeHighshelf :: MakeHighshelf
  , makeInput :: MakeInput
  , makeLoopBuf :: MakeLoopBuf
  , makeLowpass :: MakeLowpass
  , makeLowshelf :: MakeLowshelf
  , makeMediaElement :: MakeMediaElement
  , makeMicrophone :: MakeMicrophone
  , makeNotch :: MakeNotch
  , makePeaking :: MakePeaking
  , makePeriodicOsc :: MakePeriodicOsc
  , makePlayBuf :: MakePlayBuf
  , makeRecorder :: MakeRecorder
  , makeSawtoothOsc :: MakeSawtoothOsc
  , makeSinOsc :: MakeSinOsc
  , makeSquareOsc :: MakeSquareOsc
  , makeSpeaker :: MakeSpeaker
  , makeStereoPanner :: MakeStereoPanner
  , makeTriangleOsc :: MakeTriangleOsc
  , makeWaveShaper :: MakeWaveShaper
  , makeSubgraph ::
      forall index env outputChannels event payload
       . MakeSubgraph index env outputChannels event payload
  , makeTumult :: MakeTumult
  , connectXToY :: ConnectXToY
  , destroyUnit :: DestroyUnit
  , disconnectXFromY :: DisconnectXFromY
  , setAnalyserNodeCb :: SetAnalyserNodeCb
  , setMediaRecorderCb :: SetMediaRecorderCb
  , setAudioWorkletParameter :: SetAudioWorkletParameter
  , setBuffer :: SetBuffer
  , setConvolverBuffer :: SetConvolverBuffer
  , setPeriodicOsc :: SetPeriodicOsc
  , setOnOff :: SetOnOff
  , setBufferOffset :: SetBufferOffset
  , setLoopStart :: SetLoopStart
  , setLoopEnd :: SetLoopEnd
  , setRatio :: SetRatio
  , setOffset :: SetOffset
  , setAttack :: SetAttack
  , setGain :: SetGain
  , setQ :: SetQ
  , setPan :: SetPan
  , setThreshold :: SetThreshold
  , setRelease :: SetRelease
  , setKnee :: SetKnee
  , setDelay :: SetDelay
  , setPlaybackRate :: SetPlaybackRate
  , setFrequency :: SetFrequency
  , setWaveShaperCurve :: SetWaveShaperCurve
  , removeSubgraph :: forall index. RemoveSubgraph index
  , insertOrUpdateSubgraph :: forall index env. InsertOrUpdateSubgraph index env
  , setTumult :: SetTumult
  )

newtype Instruction = Instruction (Variant Instruction')

instructionWeight :: Instruction -> Int
instructionWeight (Instruction v) = v # match
  { makeAllpass: const 2
  , makeAnalyser: const 2
  , makeAudioWorkletNode: const 2
  , makeBandpass: const 2
  , makeConstant: const 2
  , makeConvolver: const 2
  , makeDelay: const 2
  , makeDynamicsCompressor: const 2
  , makeGain: const 2
  , makeHighpass: const 2
  , makeHighshelf: const 2
  , makeInput: const 2
  , makeLoopBuf: const 2
  , makeLowpass: const 2
  , makeLowshelf: const 2
  , makeMediaElement: const 2
  , makeMicrophone: const 2
  , makeNotch: const 2
  , makePeaking: const 2
  , makePeriodicOsc: const 2
  , makePlayBuf: const 2
  , makeRecorder: const 2
  , makeSawtoothOsc: const 2
  , makeSinOsc: const 2
  , makeSquareOsc: const 2
  , makeSpeaker: const 2
  , makeStereoPanner: const 2
  , makeTriangleOsc: const 2
  , makeWaveShaper: const 2
  , makeSubgraph: const 3
  , makeTumult: const 4
  , connectXToY: const 5
  , disconnectXFromY: const 0
  , destroyUnit: const 1
  , setAnalyserNodeCb: const 6
  , setMediaRecorderCb: const 6
  , setAudioWorkletParameter: const 6
  , setBuffer: const 6
  , setConvolverBuffer: const 6
  , setPeriodicOsc: const 6
  , setOnOff: const 6
  , setBufferOffset: const 6
  , setLoopStart: const 6
  , setLoopEnd: const 6
  , setRatio: const 6
  , setOffset: const 6
  , setAttack: const 6
  , setGain: const 6
  , setQ: const 6
  , setPan: const 6
  , setThreshold: const 6
  , setRelease: const 6
  , setKnee: const 6
  , setDelay: const 6
  , setPlaybackRate: const 6
  , setFrequency: const 6
  , setWaveShaperCurve: const 6
  , removeSubgraph: const 7
  , insertOrUpdateSubgraph: const 8
  , setTumult: const 9
  }

instructionId :: Instruction -> String
instructionId (Instruction v) = v # match
  { makeAllpass: _.id
  , makeAnalyser: _.id
  , makeAudioWorkletNode: _.id
  , makeBandpass: _.id
  , makeConstant: _.id
  , makeConvolver: _.id
  , makeDelay: _.id
  , makeDynamicsCompressor: _.id
  , makeGain: _.id
  , makeHighpass: _.id
  , makeHighshelf: _.id
  , makeInput: _.id
  , makeLoopBuf: _.id
  , makeLowpass: _.id
  , makeLowshelf: _.id
  , makeMediaElement: _.id
  , makeMicrophone: _.id
  , makeNotch: _.id
  , makePeaking: _.id
  , makePeriodicOsc: _.id
  , makePlayBuf: _.id
  , makeRecorder: _.id
  , makeSawtoothOsc: _.id
  , makeSinOsc: _.id
  , makeSquareOsc: _.id
  , makeSpeaker: _.id
  , makeStereoPanner: _.id
  , makeTriangleOsc: _.id
  , makeWaveShaper: _.id
  , makeSubgraph: _.id
  , makeTumult: _.id
  , connectXToY: _.from
  , disconnectXFromY: _.from
  , destroyUnit: _.id
  , setAnalyserNodeCb: _.id
  , setMediaRecorderCb: _.id
  , setAudioWorkletParameter: _.id
  , setBuffer: _.id
  , setConvolverBuffer: _.id
  , setPeriodicOsc: _.id
  , setOnOff: _.id
  , setBufferOffset: _.id
  , setLoopStart: _.id
  , setLoopEnd: _.id
  , setRatio: _.id
  , setOffset: _.id
  , setAttack: _.id
  , setGain: _.id
  , setQ: _.id
  , setPan: _.id
  , setThreshold: _.id
  , setRelease: _.id
  , setKnee: _.id
  , setDelay: _.id
  , setPlaybackRate: _.id
  , setFrequency: _.id
  , setWaveShaperCurve: _.id
  , removeSubgraph: _.id
  , insertOrUpdateSubgraph: _.id
  , setTumult: _.id
  }

instance eqInstruction :: Eq Instruction where
  eq a b = case compare a b of
    EQ -> true
    _ -> false

instance ordInstruction :: Ord Instruction where
  compare v1 v2 = case compare w1 w2 of
    EQ -> c2 unit
    x -> x
    where
    w1 = instructionWeight v1
    w2 = instructionWeight v2
    c2 _ = compare i1 i2
      where
      i1 = instructionId v1
      i2 = instructionId v2
