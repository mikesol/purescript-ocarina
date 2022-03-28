-- | Term-level representations of the typelevel graph. These are
-- | emitted by `run` and can be used to visualize the audio graph.
-- | These representations are also used interally to speed up graph
-- | rendering and to validate the graph in unit tests.
module WAGS.Rendered where

import Prelude

import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant, inj, match)
import Foreign (Foreign)
import Foreign.Object (Object)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import WAGS.Graph.Parameter (AudioParameter, AudioOnOff)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

type AudioWorkletNodeOptions_' =
  { name :: String
  , numberOfInputs :: Int
  , numberOfOutputs :: Int
  , outputChannelCount :: Array Int
  , parameterData :: Object AudioParameter
  , processorOptions :: Foreign
  }

type AudioWorkletNodeOptions_S' =
  { name :: String
  , numberOfInputs :: Int
  , numberOfOutputs :: Int
  , outputChannelCount :: Array Int
  , parameterData :: Object AudioParameter
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
  , parameterData :: Object AudioParameter
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

type DisconnectXFromY =
  { fromId :: String, fromUnit :: String, toId :: String, toUnit :: String }

type DestroyUnit = { id :: String, unit :: String }
type MakeAllpass = { id :: String, freq :: AudioParameter, q :: AudioParameter }
type MakeAnalyser = { id :: String, cb :: AnalyserNodeCb }
type MakeAudioWorkletNode =
  { id :: String, options :: AudioWorkletNodeOptions_ }

type MakeBandpass =
  { id :: String, freq :: AudioParameter, q :: AudioParameter }

type MakeConstant =
  { id :: String, onOff :: AudioOnOff, offset :: AudioParameter }

type MakePassthroughConvolver = { id :: String }
type MakeConvolver = { id :: String, buffer :: BrowserAudioBuffer }
type MakeDelay = { id :: String, delayTime :: AudioParameter }
type MakeDynamicsCompressor =
  { id :: String
  , threshold :: AudioParameter
  , knee :: AudioParameter
  , ratio :: AudioParameter
  , attack :: AudioParameter
  , release :: AudioParameter
  }

type MakeGain = { id :: String, gain :: AudioParameter }
type MakeHighpass =
  { id :: String, freq :: AudioParameter, q :: AudioParameter }

type MakeHighshelf =
  { id :: String, freq :: AudioParameter, gain :: AudioParameter }

type MakeInput = { id :: String, input :: String }
type MakeLoopBuf =
  { id :: String
  , buffer :: BrowserAudioBuffer
  , onOff :: AudioOnOff
  , playbackRate :: AudioParameter
  , loopStart :: Number
  , loopEnd :: Number
  }

type MakeLoopBufWithDeferredBuffer = { id :: String }
type MakeLowpass = { id :: String, freq :: AudioParameter, q :: AudioParameter }
type MakeLowshelf =
  { id :: String, freq :: AudioParameter, gain :: AudioParameter }

type MakeMediaElement =
  { id :: String
  , element :: BrowserMediaElement
  }

type MakeMicrophone = { microphone :: BrowserMicrophone }
type MakeNotch = { id :: String, freq :: AudioParameter, q :: AudioParameter }
type MakePeaking =
  { id :: String
  , freq :: AudioParameter
  , q :: AudioParameter
  , gain :: AudioParameter
  }

type MakePeriodicOscWithDeferredOsc = { id :: String }
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

iWave :: BrowserPeriodicWave -> PeriodicOscSpec
iWave = PeriodicOscSpec <<< inj (Proxy :: Proxy "wave")

iRealImg :: RealImg -> PeriodicOscSpec
iRealImg = PeriodicOscSpec <<< inj (Proxy :: Proxy "realImg")

derive instance newtypePeriodicOscSpec :: Newtype PeriodicOscSpec _
derive newtype instance eqPeriodicOscSpec :: Eq PeriodicOscSpec
derive newtype instance ordPeriodicOscSpec :: Ord PeriodicOscSpec
derive newtype instance showPeriodicOscSpec :: Show PeriodicOscSpec

type MakePeriodicOsc =
  { id :: String
  , spec :: PeriodicOscSpec
  , onOff :: AudioOnOff
  , freq :: AudioParameter
  }

type MakePlayBuf =
  { id :: String
  , buffer :: BrowserAudioBuffer
  , bufferOffset :: Number
  , onOff :: AudioOnOff
  , playbackRate :: AudioParameter
  }

type MakePlayBufWithDeferredBuffer = { id :: String }
type MakeRecorder = { id :: String, cb :: MediaRecorderCb }
type MakeSawtoothOsc =
  { id :: String, onOff :: AudioOnOff, freq :: AudioParameter }

type MakeSinOsc = { id :: String, onOff :: AudioOnOff, freq :: AudioParameter }
type MakeSquareOsc =
  { id :: String, onOff :: AudioOnOff, freq :: AudioParameter }

type MakeStereoPanner = { id :: String, pan :: AudioParameter }
type MakeTriangleOsc =
  { id :: String, onOff :: AudioOnOff, freq :: AudioParameter }

type MakeWaveShaper =
  { id :: String, curve :: BrowserFloatArray, oversample :: Oversample }

type MakeSubgraph =
  { id :: String, instructions :: Lazy (Array (Array Instruction)) }

type MakeTumult =
  { id :: String
  , terminus :: String
  , instructions :: Array (Array Instruction)
  }

type ConnectXToY =
  { fromId :: String, fromUnit :: String, toId :: String, toUnit :: String }

type SetAnalyserNodeCb = { id :: String, cb :: AnalyserNodeCb }
type SetMediaRecorderCb = { id :: String, cb :: MediaRecorderCb }
type SetAudioWorkletParameter =
  { id :: String, paramName :: String, paramValue :: AudioParameter }

type SetBuffer = { id :: String, buffer :: BrowserAudioBuffer }
type SetConvolverBuffer = { id :: String, buffer :: BrowserAudioBuffer }
type SetPeriodicOsc = { id :: String, periodicOsc :: PeriodicOscSpec }
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
type SetDelay = { id :: String, delay :: AudioParameter }
type SetPlaybackRate = { id :: String, playbackRate :: AudioParameter }
type SetFrequency = { id :: String, frequency :: AudioParameter }
type SetWaveShaperCurve = { id :: String, curve :: BrowserFloatArray }
type SetInput = { id :: String, source :: String }
-- TODO: we have no idea what set subgraph will hold because we
-- have no idea what the previous state was in the pure model.
-- fix? do we care? so far this is just for visualizations, but
-- if it winds up being semantically interesting we should address this
type SetSubgraph = { id :: String }
type SetTumult =
  { id :: String
  , terminus :: String
  , instructions :: Array (Array Instruction)
  }

-- An audio rendering instruction. These instructions are used
-- for testing purposes during "dry run" simulations of audio rendering.
-- `Instruction` can also be used if web-audio is being used to control other audio units.

type Instruction' =
  ( disconnectXFromY :: DisconnectXFromY
  , destroyUnit :: DestroyUnit
  , makeAllpass :: MakeAllpass
  , makeAnalyser :: MakeAnalyser
  , makeAudioWorkletNode :: MakeAudioWorkletNode
  , makeBandpass :: MakeBandpass
  , makeConstant :: MakeConstant
  , makePassthroughConvolver :: MakePassthroughConvolver
  , makeConvolver :: MakeConvolver
  , makeDelay :: MakeDelay
  , makeDynamicsCompressor :: MakeDynamicsCompressor
  , makeGain :: MakeGain
  , makeHighpass :: MakeHighpass
  , makeHighshelf :: MakeHighshelf
  , makeInput :: MakeInput
  , makeLoopBuf :: MakeLoopBuf
  , makeLoopBufWithDeferredBuffer :: MakeLoopBufWithDeferredBuffer
  , makeLowpass :: MakeLowpass
  , makeLowshelf :: MakeLowshelf
  , makeMediaElement :: MakeMediaElement
  , makeMicrophone :: MakeMicrophone
  , makeNotch :: MakeNotch
  , makePeaking :: MakePeaking
  , makePeriodicOscWithDeferredOsc :: MakePeriodicOscWithDeferredOsc
  , makePeriodicOsc :: MakePeriodicOsc
  , makePlayBuf :: MakePlayBuf
  , makePlayBufWithDeferredBuffer :: MakePlayBufWithDeferredBuffer
  , makeRecorder :: MakeRecorder
  , makeSawtoothOsc :: MakeSawtoothOsc
  , makeSinOsc :: MakeSinOsc
  , makeSquareOsc :: MakeSquareOsc
  , makeSpeaker :: Unit
  , makeStereoPanner :: MakeStereoPanner
  , makeTriangleOsc :: MakeTriangleOsc
  , makeWaveShaper :: MakeWaveShaper
  , makeSubgraph :: MakeSubgraph
  , makeTumult :: MakeTumult
  , connectXToY :: ConnectXToY
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
  , setInput :: SetInput
  , setSubgraph :: SetSubgraph
  , setTumult :: SetTumult
  )

newtype Instruction = Instruction (Variant Instruction')

instructionWeight :: Instruction -> Int
instructionWeight (Instruction v) = v # match
  { disconnectXFromY: const 0
  , destroyUnit: const 1
  , makeAllpass: const 2
  , makeAnalyser: const 2
  , makeAudioWorkletNode: const 2
  , makeBandpass: const 2
  , makeConstant: const 2
  , makePassthroughConvolver: const 2
  , makeConvolver: const 2
  , makeDelay: const 2
  , makeDynamicsCompressor: const 2
  , makeGain: const 2
  , makeHighpass: const 2
  , makeHighshelf: const 2
  , makeInput: const 2
  , makeLoopBuf: const 2
  , makeLoopBufWithDeferredBuffer: const 2
  , makeLowpass: const 2
  , makeLowshelf: const 2
  , makeMediaElement: const 2
  , makeMicrophone: const 2
  , makeNotch: const 2
  , makePeaking: const 2
  , makePeriodicOscWithDeferredOsc: const 2
  , makePeriodicOsc: const 2
  , makePlayBuf: const 2
  , makePlayBufWithDeferredBuffer: const 2
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
  , setInput: const 6
  , setSubgraph: const 7
  , setTumult: const 9
  }

instructionId :: Instruction -> String
instructionId (Instruction v) = v # match
  { disconnectXFromY: _.fromId
  , destroyUnit: _.id
  , makeAllpass: _.id
  , makeAnalyser: _.id
  , makeAudioWorkletNode: _.id
  , makeBandpass: _.id
  , makeConstant: _.id
  , makePassthroughConvolver: _.id
  , makeConvolver: _.id
  , makeDelay: _.id
  , makeDynamicsCompressor: _.id
  , makeGain: _.id
  , makeHighpass: _.id
  , makeHighshelf: _.id
  , makeInput: _.id
  , makeLoopBuf: _.id
  , makeLoopBufWithDeferredBuffer: _.id
  , makeLowpass: _.id
  , makeLowshelf: _.id
  , makeMediaElement: _.id
  , makeMicrophone: const "microphone"
  , makeNotch: _.id
  , makePeaking: _.id
  , makePeriodicOscWithDeferredOsc: _.id
  , makePeriodicOsc: _.id
  , makePlayBuf: _.id
  , makePlayBufWithDeferredBuffer: _.id
  , makeRecorder: _.id
  , makeSawtoothOsc: _.id
  , makeSinOsc: _.id
  , makeSquareOsc: _.id
  , makeSpeaker: const "speaker"
  , makeStereoPanner: _.id
  , makeTriangleOsc: _.id
  , makeWaveShaper: _.id
  , makeSubgraph: _.id
  , makeTumult: _.id
  , connectXToY: _.fromId
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
  , setInput: _.id
  , setSubgraph: _.id
  , setTumult: _.id
  }

derive instance newtypeInstruction :: Newtype Instruction _
derive instance eqInstruction :: Eq Instruction
instance ordInstruction :: Ord Instruction where
  compare v1@(Instruction v1') v2@(Instruction v2') = case compare w1 w2 of
    EQ -> c2 unit
    x -> x
    where
    w1 = instructionWeight v1
    w2 = instructionWeight v2
    c2 _ = case compare i1 i2 of
      EQ -> compare v1' v2'
      x -> x
      where
      i1 = instructionId v1
      i2 = instructionId v2

instance showInstruction :: Show Instruction where
  show (Instruction i) = show i

iDisconnectXFromY :: DisconnectXFromY -> Instruction
iDisconnectXFromY = Instruction <<< inj (Proxy :: Proxy "disconnectXFromY")

iDestroyUnit :: DestroyUnit -> Instruction
iDestroyUnit = Instruction <<< inj (Proxy :: Proxy "destroyUnit")

iMakeAllpass :: MakeAllpass -> Instruction
iMakeAllpass = Instruction <<< inj (Proxy :: Proxy "makeAllpass")

iMakeAnalyser :: MakeAnalyser -> Instruction
iMakeAnalyser = Instruction <<< inj (Proxy :: Proxy "makeAnalyser")

iMakeAudioWorkletNode :: MakeAudioWorkletNode -> Instruction
iMakeAudioWorkletNode = Instruction <<< inj
  (Proxy :: Proxy "makeAudioWorkletNode")

iMakeBandpass :: MakeBandpass -> Instruction
iMakeBandpass = Instruction <<< inj (Proxy :: Proxy "makeBandpass")

iMakeConstant :: MakeConstant -> Instruction
iMakeConstant = Instruction <<< inj (Proxy :: Proxy "makeConstant")

iMakePassthroughConvolver :: MakePassthroughConvolver -> Instruction
iMakePassthroughConvolver = Instruction <<< inj
  (Proxy :: Proxy "makePassthroughConvolver")

iMakeConvolver :: MakeConvolver -> Instruction
iMakeConvolver = Instruction <<< inj (Proxy :: Proxy "makeConvolver")

iMakeDelay :: MakeDelay -> Instruction
iMakeDelay = Instruction <<< inj (Proxy :: Proxy "makeDelay")

iMakeDynamicsCompressor :: MakeDynamicsCompressor -> Instruction
iMakeDynamicsCompressor = Instruction <<< inj
  (Proxy :: Proxy "makeDynamicsCompressor")

iMakeGain :: MakeGain -> Instruction
iMakeGain = Instruction <<< inj (Proxy :: Proxy "makeGain")

iMakeHighpass :: MakeHighpass -> Instruction
iMakeHighpass = Instruction <<< inj (Proxy :: Proxy "makeHighpass")

iMakeHighshelf :: MakeHighshelf -> Instruction
iMakeHighshelf = Instruction <<< inj (Proxy :: Proxy "makeHighshelf")

iMakeInput :: MakeInput -> Instruction
iMakeInput = Instruction <<< inj (Proxy :: Proxy "makeInput")

iMakeLoopBuf :: MakeLoopBuf -> Instruction
iMakeLoopBuf = Instruction <<< inj (Proxy :: Proxy "makeLoopBuf")

iMakeLoopBufWithDeferredBuffer :: MakeLoopBufWithDeferredBuffer -> Instruction
iMakeLoopBufWithDeferredBuffer = Instruction <<< inj
  (Proxy :: Proxy "makeLoopBufWithDeferredBuffer")

iMakeLowpass :: MakeLowpass -> Instruction
iMakeLowpass = Instruction <<< inj (Proxy :: Proxy "makeLowpass")

iMakeLowshelf :: MakeLowshelf -> Instruction
iMakeLowshelf = Instruction <<< inj (Proxy :: Proxy "makeLowshelf")

iMakeMediaElement :: MakeMediaElement -> Instruction
iMakeMediaElement = Instruction <<< inj (Proxy :: Proxy "makeMediaElement")

iMakeMicrophone :: MakeMicrophone -> Instruction
iMakeMicrophone = Instruction <<< inj (Proxy :: Proxy "makeMicrophone")

iMakeNotch :: MakeNotch -> Instruction
iMakeNotch = Instruction <<< inj (Proxy :: Proxy "makeNotch")

iMakePeaking :: MakePeaking -> Instruction
iMakePeaking = Instruction <<< inj (Proxy :: Proxy "makePeaking")

iMakePeriodicOscWithDeferredOsc :: MakePeriodicOscWithDeferredOsc -> Instruction
iMakePeriodicOscWithDeferredOsc = Instruction <<< inj
  (Proxy :: Proxy "makePeriodicOscWithDeferredOsc")

iMakePeriodicOsc :: MakePeriodicOsc -> Instruction
iMakePeriodicOsc = Instruction <<< inj (Proxy :: Proxy "makePeriodicOsc")

iMakePlayBuf :: MakePlayBuf -> Instruction
iMakePlayBuf = Instruction <<< inj (Proxy :: Proxy "makePlayBuf")

iMakePlayBufWithDeferredBuffer :: MakePlayBufWithDeferredBuffer -> Instruction
iMakePlayBufWithDeferredBuffer = Instruction <<< inj
  (Proxy :: Proxy "makePlayBufWithDeferredBuffer")

iMakeRecorder :: MakeRecorder -> Instruction
iMakeRecorder = Instruction <<< inj (Proxy :: Proxy "makeRecorder")

iMakeSawtoothOsc :: MakeSawtoothOsc -> Instruction
iMakeSawtoothOsc = Instruction <<< inj (Proxy :: Proxy "makeSawtoothOsc")

iMakeSinOsc :: MakeSinOsc -> Instruction
iMakeSinOsc = Instruction <<< inj (Proxy :: Proxy "makeSinOsc")

iMakeSquareOsc :: MakeSquareOsc -> Instruction
iMakeSquareOsc = Instruction <<< inj (Proxy :: Proxy "makeSquareOsc")

iMakeSpeaker :: Instruction
iMakeSpeaker = Instruction $ inj (Proxy :: Proxy "makeSpeaker") unit

iMakeStereoPanner :: MakeStereoPanner -> Instruction
iMakeStereoPanner = Instruction <<< inj (Proxy :: Proxy "makeStereoPanner")

iMakeTriangleOsc :: MakeTriangleOsc -> Instruction
iMakeTriangleOsc = Instruction <<< inj (Proxy :: Proxy "makeTriangleOsc")

iMakeWaveShaper :: MakeWaveShaper -> Instruction
iMakeWaveShaper = Instruction <<< inj (Proxy :: Proxy "makeWaveShaper")

iMakeSubgraph :: MakeSubgraph -> Instruction
iMakeSubgraph = Instruction <<< inj (Proxy :: Proxy "makeSubgraph")

iMakeTumult :: MakeTumult -> Instruction
iMakeTumult = Instruction <<< inj (Proxy :: Proxy "makeTumult")

iConnectXToY :: ConnectXToY -> Instruction
iConnectXToY = Instruction <<< inj (Proxy :: Proxy "connectXToY")

iSetAnalyserNodeCb :: SetAnalyserNodeCb -> Instruction
iSetAnalyserNodeCb = Instruction <<< inj (Proxy :: Proxy "setAnalyserNodeCb")

iSetMediaRecorderCb :: SetMediaRecorderCb -> Instruction
iSetMediaRecorderCb = Instruction <<< inj (Proxy :: Proxy "setMediaRecorderCb")

iSetAudioWorkletParameter :: SetAudioWorkletParameter -> Instruction
iSetAudioWorkletParameter = Instruction <<< inj
  (Proxy :: Proxy "setAudioWorkletParameter")

iSetBuffer :: SetBuffer -> Instruction
iSetBuffer = Instruction <<< inj (Proxy :: Proxy "setBuffer")

iSetConvolverBuffer :: SetConvolverBuffer -> Instruction
iSetConvolverBuffer = Instruction <<< inj (Proxy :: Proxy "setConvolverBuffer")

iSetPeriodicOsc :: SetPeriodicOsc -> Instruction
iSetPeriodicOsc = Instruction <<< inj (Proxy :: Proxy "setPeriodicOsc")

iSetOnOff :: SetOnOff -> Instruction
iSetOnOff = Instruction <<< inj (Proxy :: Proxy "setOnOff")

iSetBufferOffset :: SetBufferOffset -> Instruction
iSetBufferOffset = Instruction <<< inj (Proxy :: Proxy "setBufferOffset")

iSetLoopStart :: SetLoopStart -> Instruction
iSetLoopStart = Instruction <<< inj (Proxy :: Proxy "setLoopStart")

iSetLoopEnd :: SetLoopEnd -> Instruction
iSetLoopEnd = Instruction <<< inj (Proxy :: Proxy "setLoopEnd")

iSetRatio :: SetRatio -> Instruction
iSetRatio = Instruction <<< inj (Proxy :: Proxy "setRatio")

iSetOffset :: SetOffset -> Instruction
iSetOffset = Instruction <<< inj (Proxy :: Proxy "setOffset")

iSetAttack :: SetAttack -> Instruction
iSetAttack = Instruction <<< inj (Proxy :: Proxy "setAttack")

iSetGain :: SetGain -> Instruction
iSetGain = Instruction <<< inj (Proxy :: Proxy "setGain")

iSetQ :: SetQ -> Instruction
iSetQ = Instruction <<< inj (Proxy :: Proxy "setQ")

iSetPan :: SetPan -> Instruction
iSetPan = Instruction <<< inj (Proxy :: Proxy "setPan")

iSetThreshold :: SetThreshold -> Instruction
iSetThreshold = Instruction <<< inj (Proxy :: Proxy "setThreshold")

iSetRelease :: SetRelease -> Instruction
iSetRelease = Instruction <<< inj (Proxy :: Proxy "setRelease")

iSetKnee :: SetKnee -> Instruction
iSetKnee = Instruction <<< inj (Proxy :: Proxy "setKnee")

iSetDelay :: SetDelay -> Instruction
iSetDelay = Instruction <<< inj (Proxy :: Proxy "setDelay")

iSetPlaybackRate :: SetPlaybackRate -> Instruction
iSetPlaybackRate = Instruction <<< inj (Proxy :: Proxy "setPlaybackRate")

iSetFrequency :: SetFrequency -> Instruction
iSetFrequency = Instruction <<< inj (Proxy :: Proxy "setFrequency")

iSetWaveShaperCurve :: SetWaveShaperCurve -> Instruction
iSetWaveShaperCurve = Instruction <<< inj (Proxy :: Proxy "setWaveShaperCurve")

iSetInput :: SetInput -> Instruction
iSetInput = Instruction <<< inj (Proxy :: Proxy "setInput")

iSetSubgraph :: SetSubgraph -> Instruction
iSetSubgraph = Instruction <<< inj (Proxy :: Proxy "setSubgraph")

iSetTumult :: SetTumult -> Instruction
iSetTumult = Instruction <<< inj (Proxy :: Proxy "setTumult")

-- | The amount a [WaveShaperNode](https://developer.mozilla.org/en-US/docs/Web/API/WaveShaperNode) should oversample.
newtype Oversample = Oversample
  ( Variant
      ( none :: Unit
      , "2x" :: Unit
      , "4x" :: Unit
      )
  )

derive instance newtypeOversample :: Newtype Oversample _

oNone :: Oversample
oNone = Oversample (inj (Proxy :: Proxy "none") unit)

oTwoX :: Oversample
oTwoX = Oversample (inj (Proxy :: Proxy "2x") unit)

oFourX :: Oversample
oFourX = Oversample (inj (Proxy :: Proxy "4x") unit)

derive instance eqOversample :: Eq Oversample

derive instance ordOversample :: Ord Oversample

derive instance genericOversample :: Generic Oversample _

instance showOversample :: Show Oversample where
  show = genericShow
