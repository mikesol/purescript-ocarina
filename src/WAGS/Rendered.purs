-- | Term-level representations of the typelevel graph. These are
-- | emitted by `run` and can be used to visualize the audio graph.
-- | These representations are also used interally to speed up graph
-- | rendering and to validate the graph in unit tests.
module WAGS.Rendered where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Foreign (Foreign)
import Simple.JSON as JSON
import WAGS.Graph.AudioUnit (APOnOff)
import WAGS.Graph.Parameter (AudioParameter)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

newtype AudioWorkletNodeOptions_ = AudioWorkletNodeOptions_ Foreign

derive instance newtypeAudioWorkletNodeOptions_ :: Newtype AudioWorkletNodeOptions_ _

instance eqAudioWorkletNodeOptions_ :: Eq AudioWorkletNodeOptions_ where
  eq a b = JSON.writeJSON (unwrap a) == JSON.writeJSON (unwrap b)

instance showAudioWorkletNodeOptions_ :: Show AudioWorkletNodeOptions_ where
  show = JSON.writeJSON <<< unwrap

-- An audio rendering instruction. These instructions are used
-- for testing purposes during "dry run" simulations of audio rendering.
-- `Instruction` can also be used if web-audio is being used to control other audio units.
data Instruction
  = ConnectXToY String String
  | DisconnectXFromY String String
  | DestroyUnit String
  | MakeAllpass String AudioParameter AudioParameter
  | MakeAnalyser String AnalyserNodeCb
  | MakeAudioWorkletNode String String AudioWorkletNodeOptions_
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
  -- for now, MakeSubgraph intentionally leaves off the scene part
  | MakeSubgraph String
  -- for now, MakeSubgraphWithDeferredScene intentionally leaves off the scene part
  | MakeSubgraphWithDeferredScene String
  | MakeTriangleOsc String APOnOff AudioParameter
  | MakeWaveShaper String BrowserFloatArray Oversample
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
  -- for now, SetSubgraph intentionally leaves off the scene part
  | SetSubgraph String

derive instance eqInstruction :: Eq Instruction

derive instance genericInstruction :: Generic Instruction _

instance showInstruction :: Show Instruction where
  show = genericShow

-- | The amount a [WaveShaperNode](https://developer.mozilla.org/en-US/docs/Web/API/WaveShaperNode) should oversample.
data Oversample
  = None
  | TwoX
  | FourX

derive instance eqOversample :: Eq Oversample

derive instance genericOversample :: Generic Oversample _

instance showOversample :: Show Oversample where
  show = genericShow
