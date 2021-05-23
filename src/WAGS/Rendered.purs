-- | Term-level representations of the typelevel graph. These are
-- | emitted by `run` and can be used to visualize the audio graph.
-- | These representations are also used interally to speed up graph
-- | rendering and to validate the graph in unit tests.
module WAGS.Rendered where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import WAGS.Graph.AudioUnit (OnOff)
import WAGS.Graph.Parameter (AudioParameter)

-- An audio rendering instruction. These instructions are used
-- for testing purposes during "dry run" simulations of audio rendering.
-- `Instruction` can also be used if web-audio is being used to control other audio units.
data Instruction
  = ConnectXToY String String
  | DisconnectXFromY String String
  | DestroyUnit String
  | MakeAllpass String AudioParameter AudioParameter
  | MakeBandpass String AudioParameter AudioParameter
  | MakeConstant String OnOff AudioParameter
  | MakeConvolver String String
  | MakeDelay String AudioParameter
  | MakeDynamicsCompressor String AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter
  | MakeGain String AudioParameter
  | MakeHighpass String AudioParameter AudioParameter
  | MakeHighshelf String AudioParameter AudioParameter
  | MakeLoopBuf String String OnOff AudioParameter Number Number
  | MakeLoopBufWithDeferredBuffer String
  | MakeLowpass String AudioParameter AudioParameter
  | MakeLowshelf String AudioParameter AudioParameter
  | MakeMicrophone
  | MakeNotch String AudioParameter AudioParameter
  | MakePeaking String AudioParameter AudioParameter AudioParameter
  | MakePeriodicOscWithDeferredOsc String
  | MakePeriodicOsc String String OnOff AudioParameter
  | MakePlayBuf String String Number OnOff AudioParameter
  | MakePlayBufWithDeferredBuffer String
  | MakeRecorder String String
  | MakeSawtoothOsc String OnOff AudioParameter
  | MakeSinOsc String OnOff AudioParameter
  | MakeSquareOsc String OnOff AudioParameter
  | MakeSpeaker
  | MakeStereoPanner String AudioParameter
  | MakeTriangleOsc String OnOff AudioParameter
  | MakeWaveShaper String String Oversample
  | SetBuffer String String
  | SetPeriodicOsc String String
  | SetOn String
  | SetOff String
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

-- | A term-level representation of audio units. Used for testing as well as internally to
-- | represent the audio graph. They are also present in the output of `run` for downstream
-- | graph visualization.
data AnAudioUnit
  = AAllpass AudioParameter AudioParameter
  | ABandpass AudioParameter AudioParameter
  | AConstant OnOff AudioParameter
  | AConvolver String
  | ADelay AudioParameter
  | ADynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter
  | AGain AudioParameter
  | AHighpass AudioParameter AudioParameter
  | AHighshelf AudioParameter AudioParameter
  | ALoopBuf String OnOff AudioParameter Number Number
  | ALowpass AudioParameter AudioParameter
  | ALowshelf AudioParameter AudioParameter
  | AMicrophone
  | ANotch AudioParameter AudioParameter
  | APeaking AudioParameter AudioParameter AudioParameter
  | APeriodicOsc String OnOff AudioParameter
  | APlayBuf String Number OnOff AudioParameter
  | ARecorder String
  | ASawtoothOsc OnOff AudioParameter
  | ASinOsc OnOff AudioParameter
  | ASpeaker
  | ASquareOsc OnOff AudioParameter
  | AStereoPanner AudioParameter
  | ATriangleOsc OnOff AudioParameter
  | AWaveShaper String Oversample

derive instance eqAnAudioUnit :: Eq AnAudioUnit

derive instance genericAnAudioUnit :: Generic AnAudioUnit _

instance showAnAudioUnit :: Show AnAudioUnit where
  show = genericShow
