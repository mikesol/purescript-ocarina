module WAGS.Core where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant, inj, match)
import Data.Variant.Maybe (Maybe)
import FRP.Behavior (ABehavior)
import FRP.Event.Pure (PureEvent)
import Foreign (Foreign)
import Foreign.Object (Object)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import WAGS.Parameter (AudioOnOff, AudioParameter, InitialAudioParameter)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

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

derive instance newtypeInput :: Newtype Input _
newtype Input = Input String

newtype Node outputChannels produced consumed event payload = Node
  (String -> AudioInterpret event payload -> event payload)

newtype GainInput outputChannels produced consumed event payload = GainInput
  (NonEmptyArray (Node outputChannels produced consumed event payload))

type SubgraphSig index env outputChannels sgProduced sgConsumed event payload =
  index
  -> event env
  -> Node outputChannels sgProduced sgConsumed event payload

newtype Subgraph index env outputChannels sgProduced sgConsumed event payload =
  Subgraph
    ( index
      -> event env
      -> Node outputChannels sgProduced sgConsumed event payload
    )

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

type DestroyUnit_ = (id :: String)
type DestroyUnit = { | DestroyUnit_ }
type DestroyUnit' = { unit :: String | DestroyUnit_ }

derive instance newtypeAllpass :: Newtype Allpass _
newtype Allpass = Allpass
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
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
type MakeAllpass' = MakeAllpass_ AudioParameter

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
type MakeAudioWorkletNode' = MakeAudioWorkletNode_ AudioParameter

derive instance newtypeBandpass :: Newtype Bandpass _
newtype Bandpass = Bandpass
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
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
type MakeBandpass' = MakeBandpass_ AudioParameter

derive instance newtypeConstant :: Newtype Constant _
newtype Constant = Constant
  (Variant (offset :: AudioParameter, onOff :: AudioOnOff))

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
type MakeConstant' = { onOff :: AudioOnOff | MakeConstant_ AudioParameter }

derive instance newtypeInitializeConvolver :: Newtype InitializeConvolver _
newtype InitializeConvolver = InitializeConvolver
  { buffer :: BrowserAudioBuffer }

type MakeConvolver =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , buffer :: BrowserAudioBuffer
  }

derive instance newtypeDelay :: Newtype Delay _
newtype Delay = Delay (Variant (delayTime :: AudioParameter))

derive instance newtypeInitializeDelay :: Newtype InitializeDelay _
newtype InitializeDelay = InitializeDelay { delayTime :: InitialAudioParameter }
type MakeDelay_ param =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , delayTime :: param
  }

type MakeDelay = MakeDelay_ InitialAudioParameter
type MakeDelay' = MakeDelay_ AudioParameter

derive instance newtypeDynamicsCompressor :: Newtype DynamicsCompressor _
newtype DynamicsCompressor = DynamicsCompressor
  ( Variant
      ( threshold :: AudioParameter
      , knee :: AudioParameter
      , ratio :: AudioParameter
      , attack :: AudioParameter
      , release :: AudioParameter
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
type MakeDynamicsCompressor' = MakeDynamicsCompressor_ AudioParameter

derive instance newtypeGain :: Newtype Gain _
newtype Gain = Gain (Variant (gain :: AudioParameter))

derive instance newtypeInitializeGain :: Newtype InitializeGain _
newtype InitializeGain = InitializeGain { gain :: InitialAudioParameter }
type MakeGain_ param =
  { id :: String, parent :: Maybe String, scope :: Maybe String, gain :: param }

type MakeGain = MakeGain_ InitialAudioParameter
type MakeGain' = MakeGain_ AudioParameter

derive instance newtypeHighpass :: Newtype Highpass _
newtype Highpass = Highpass
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
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
type MakeHighpass' = MakeHighpass_ AudioParameter

derive instance newtypeHighshelf :: Newtype Highshelf _
newtype Highshelf = Highshelf
  ( Variant
      ( frequency :: AudioParameter
      , gain :: AudioParameter
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
type MakeHighshelf' = MakeHighshelf_ AudioParameter

type MakeInput = { id :: String, parent :: Maybe String, scope :: Maybe String }
type MakeTumultInput = { id :: String, input :: String }

derive instance newtypeLoopBuf :: Newtype LoopBuf _
newtype LoopBuf = LoopBuf
  ( Variant
      ( buffer :: BrowserAudioBuffer
      , onOff :: AudioOnOff
      , playbackRate :: AudioParameter
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
type MakeLoopBuf' = { onOff :: AudioOnOff | MakeLoopBuf_ AudioParameter }

derive instance newtypeLowpass :: Newtype Lowpass _
newtype Lowpass = Lowpass
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
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
type MakeLowpass' = MakeLowpass_ AudioParameter

derive instance newtypeLowshelf :: Newtype Lowshelf _
newtype Lowshelf = Lowshelf
  ( Variant
      ( frequency :: AudioParameter
      , gain :: AudioParameter
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
type MakeLowshelf' = MakeLowshelf_ AudioParameter

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
  , parent :: Maybe String
  , scope :: Maybe String
  }

type MakeMicrophone =
  { id :: String
  , microphone :: BrowserMicrophone
  , parent :: Maybe String
  , scope :: Maybe String
  }

derive instance newtypeNotch :: Newtype Notch _
newtype Notch = Notch
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
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
type MakeNotch' = MakeNotch_ AudioParameter

derive instance newtypePeaking :: Newtype Peaking _
newtype Peaking = Peaking
  ( Variant
      ( frequency :: AudioParameter
      , q :: AudioParameter
      , gain :: AudioParameter
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
type MakePeaking' = MakePeaking_ AudioParameter

derive instance newtypePeriodicOsc :: Newtype PeriodicOsc _
newtype PeriodicOsc =
  PeriodicOsc
    ( Variant
        ( spec :: PeriodicOscSpec
        , onOff :: AudioOnOff
        , frequency :: AudioParameter
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
type MakePeriodicOsc' =
  { onOff :: AudioOnOff | MakePeriodicOsc_ AudioParameter }

derive instance newtypePlayBuf :: Newtype PlayBuf _
newtype PlayBuf =
  PlayBuf
    ( Variant
        ( buffer :: BrowserAudioBuffer
        , bufferOffset :: Number
        , onOff :: AudioOnOff
        , playbackRate :: AudioParameter
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
type MakePlayBuf' = { onOff :: AudioOnOff | MakePlayBuf_ AudioParameter }

derive instance newtypeInitializeRecorder :: Newtype InitializeRecorder _
newtype InitializeRecorder = InitializeRecorder { cb :: MediaRecorderCb }
type MakeRecorder =
  { id :: String
  , parent :: Maybe String
  , scope :: Maybe String
  , cb :: MediaRecorderCb
  }

type MakeSpeaker = { id :: String }

derive instance newtypeSawtoothOsc :: Newtype SawtoothOsc _
newtype SawtoothOsc =
  SawtoothOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter
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
type MakeSawtoothOsc' =
  { onOff :: AudioOnOff | MakeSawtoothOsc_ AudioParameter }

derive instance newtypeSinOsc :: Newtype SinOsc _
newtype SinOsc = SinOsc
  (Variant (frequency :: AudioParameter, onOff :: AudioOnOff))

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
type MakeSinOsc' = { onOff :: AudioOnOff | MakeSinOsc_ AudioParameter }

derive instance newtypeSquareOsc :: Newtype SquareOsc _
newtype SquareOsc =
  SquareOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter
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
type MakeSquareOsc' = { onOff :: AudioOnOff | MakeSquareOsc_ AudioParameter }

derive instance newtypeStereoPanner :: Newtype StereoPanner _
newtype StereoPanner =
  StereoPanner (Variant (pan :: AudioParameter))

derive instance newtypeInitializeStereoPanner ::
  Newtype InitializeStereoPanner _

newtype InitializeStereoPanner = InitializeStereoPanner
  { pan :: InitialAudioParameter }

type MakeStereoPanner_ param =
  { id :: String, parent :: Maybe String, scope :: Maybe String, pan :: param }

type MakeStereoPanner = MakeStereoPanner_ InitialAudioParameter
type MakeStereoPanner' = MakeStereoPanner_ AudioParameter

derive instance newtypeTriangleOsc :: Newtype TriangleOsc _
newtype TriangleOsc =
  TriangleOsc
    ( Variant
        ( onOff :: AudioOnOff
        , frequency :: AudioParameter
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
type MakeTriangleOsc' =
  { onOff :: AudioOnOff | MakeTriangleOsc_ AudioParameter }

derive instance newtypeInitializeWaveshaper :: Newtype InitializeWaveshaper _
newtype InitializeWaveshaper = InitializeWaveshaper
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

newtype Tumult =
  Tumuilt
    ( Variant
        (instructions :: forall r. PureEvent r Unit -> PureEvent r Instruction)
    )

newtype InitializeTumult = InitializeTumult
  { instructions :: forall r. PureEvent r Unit -> PureEvent r Instruction
  }

type MakeTumult =
  { id :: String
  , parent :: String
  , scope :: String
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
  (sgProduced :: Row Type)
  (sgConsumed :: Row Type)
  event
  payload =
  { id :: String
  , parent :: String
  , scope :: String
  , scenes ::
      Subgraph index env outputChannels sgProduced sgConsumed event payload
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
  { scope :: String
  , ids :: ABehavior (event) String
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
      forall index env outputChannels sgProduced sgConsumed
       . MakeSubgraph index env outputChannels sgProduced sgConsumed event
           payload
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
  ( makeAllpass :: MakeAllpass'
  , makeAnalyser :: MakeAnalyser
  , makeAudioWorkletNode :: MakeAudioWorkletNode'
  , makeBandpass :: MakeBandpass'
  , makeConstant :: MakeConstant'
  , makeConvolver :: MakeConvolver
  , makeDelay :: MakeDelay'
  , makeDynamicsCompressor :: MakeDynamicsCompressor'
  , makeGain :: MakeGain'
  , makeHighpass :: MakeHighpass'
  , makeHighshelf :: MakeHighshelf'
  , makeLoopBuf :: MakeLoopBuf'
  , makeLowpass :: MakeLowpass'
  , makeLowshelf :: MakeLowshelf'
  , makeInput :: MakeTumultInput
  , makeMediaElement :: MakeMediaElement
  , makeMicrophone :: MakeMicrophone
  , makeNotch :: MakeNotch'
  , makePeaking :: MakePeaking'
  , makePeriodicOsc :: MakePeriodicOsc'
  , makePlayBuf :: MakePlayBuf'
  , makeRecorder :: MakeRecorder
  , makeSawtoothOsc :: MakeSawtoothOsc'
  , makeSinOsc :: MakeSinOsc'
  , makeSquareOsc :: MakeSquareOsc'
  , makeStereoPanner :: MakeStereoPanner'
  , makeTriangleOsc :: MakeTriangleOsc'
  , makeWaveShaper :: MakeWaveShaper
  , connectXToY :: ConnectXToY'
  , destroyUnit :: DestroyUnit'
  , disconnectXFromY :: DisconnectXFromY'
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
  , makeStereoPanner: const 2
  , makeTriangleOsc: const 2
  , makeWaveShaper: const 2
  , makeInput: const 3
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
  , makeStereoPanner: _.id
  , makeTriangleOsc: _.id
  , makeWaveShaper: _.id
  , makeInput: _.id
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
