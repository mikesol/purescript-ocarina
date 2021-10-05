-- | Term-level representations of the typelevel graph. These are
-- | emitted by `run` and can be used to visualize the audio graph.
-- | These representations are also used interally to speed up graph
-- | rendering and to validate the graph in unit tests.
module WAGS.Rendered where

import Prelude

import Data.Either (Either)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Foreign (Foreign)
import Foreign.Object (Object)
import Simple.JSON as JSON
import WAGS.Graph.AudioUnit (APOnOff)
import WAGS.Graph.Parameter (AudioParameter)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

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

audioWorkletNodeOptionsForInstances :: AudioWorkletNodeOptions_' -> AudioWorkletNodeOptions_S'
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

derive instance newtypeAudioWorkletNodeOptions_ :: Newtype AudioWorkletNodeOptions_ _

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

-- An audio rendering instruction. These instructions are used
-- for testing purposes during "dry run" simulations of audio rendering.
-- `Instruction` can also be used if web-audio is being used to control other audio units.
data Instruction
  = DisconnectXFromY String String String String
  | DestroyUnit String String
  | MakeAllpass String AudioParameter AudioParameter
  | MakeAnalyser String AnalyserNodeCb
  | MakeAudioWorkletNode String AudioWorkletNodeOptions_
  | MakeBandpass String AudioParameter AudioParameter
  | MakeConstant String APOnOff AudioParameter
  | MakePassthroughConvolver String
  | MakeConvolver String BrowserAudioBuffer
  | MakeDelay String AudioParameter
  | MakeDynamicsCompressor String AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter
  | MakeGain String AudioParameter
  | MakeHighpass String AudioParameter AudioParameter
  | MakeHighshelf String AudioParameter AudioParameter
  | MakeInput String String
  | MakeLoopBuf String BrowserAudioBuffer APOnOff AudioParameter Number Number
  | MakeLoopBufWithDeferredBuffer String
  | MakeLowpass String AudioParameter AudioParameter
  | MakeLowshelf String AudioParameter AudioParameter
  | MakeMicrophone BrowserMicrophone
  | MakeNotch String AudioParameter AudioParameter
  | MakePeaking String AudioParameter AudioParameter AudioParameter
  | MakePeriodicOscWithDeferredOsc String
  | MakePeriodicOsc String (Either BrowserPeriodicWave (Array Number /\ Array Number)) APOnOff AudioParameter
  | MakePlayBuf String BrowserAudioBuffer Number APOnOff AudioParameter
  | MakePlayBufWithDeferredBuffer String
  | MakeRecorder String MediaRecorderCb
  | MakeSawtoothOsc String APOnOff AudioParameter
  | MakeSinOsc String APOnOff AudioParameter
  | MakeSquareOsc String APOnOff AudioParameter
  | MakeSpeaker
  | MakeStereoPanner String AudioParameter
  | MakeTriangleOsc String APOnOff AudioParameter
  | MakeWaveShaper String BrowserFloatArray Oversample
  | MakeSubgraph String (Lazy (Array (Array Instruction)))
  | MakeSubgraphWithDeferredScene String
  | MakeTumult String String (Array (Set Instruction))
  | MakeTumultWithDeferredGraph String
  | ConnectXToY String String String String
  | SetAnalyserNodeCb String AnalyserNodeCb
  | SetMediaRecorderCb String MediaRecorderCb
  | SetAudioWorkletParameter String String AudioParameter
  | SetBuffer String BrowserAudioBuffer
  | SetConvolverBuffer String BrowserAudioBuffer
  | SetPeriodicOsc String (Either BrowserPeriodicWave (Array Number /\ Array Number))
  | SetOnOff String APOnOff
  | SetBufferOffset String Number
  | SetLoopStart String Number
  | SetLoopEnd String Number
  | SetRatio String AudioParameter
  | SetOffset String AudioParameter
  | SetAttack String AudioParameter
  | SetGain String AudioParameter
  | SetQ String AudioParameter
  | SetPan String AudioParameter
  | SetThreshold String AudioParameter
  | SetRelease String AudioParameter
  | SetKnee String AudioParameter
  | SetDelay String AudioParameter
  | SetPlaybackRate String AudioParameter
  | SetFrequency String AudioParameter
  | SetWaveShaperCurve String BrowserFloatArray
  | SetInput String String
  | SetSubgraph String (Lazy (Array (Array Instruction)))
  | SetTumult String String (Array (Set Instruction))

derive instance eqInstruction :: Eq Instruction

derive instance ordInstruction :: Ord Instruction

derive instance genericInstruction :: Generic Instruction _

instance showInstruction :: Show Instruction where
  show x = genericShow x

-- | The amount a [WaveShaperNode](https://developer.mozilla.org/en-US/docs/Web/API/WaveShaperNode) should oversample.
data Oversample
  = None
  | TwoX
  | FourX

derive instance eqOversample :: Eq Oversample

derive instance ordOversample :: Ord Oversample

derive instance genericOversample :: Generic Oversample _

instance showOversample :: Show Oversample where
  show = genericShow
