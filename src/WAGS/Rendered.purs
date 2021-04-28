-- | Term-level representations of the typelevel graph. These are
-- | emitted by `run` and can be used to visualize the audio graph.
-- | These representations are also used interally to speed up graph
-- | rendering and to validate the graph in unit tests.
module WAGS.Rendered where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import WAGS.Graph.Constructors (OnOff)
import WAGS.Graph.Parameter (AudioParameter)

-- An audio rendering instruction. These instructions are used
-- for testing purposes during "dry run" simulations of audio rendering.
-- `Instruction` can also be used if web-audio is being used to control other audio units.
data Instruction
  = ConnectXToY Int Int
  | DisconnectXFromY Int Int
  | DestroyUnit Int
  | RebaseAllUnits (Array { from :: Int, to :: Int })
  | MakeAllpass Int AudioParameter AudioParameter
  | MakeBandpass Int AudioParameter AudioParameter
  | MakeConstant Int OnOff AudioParameter
  | MakeConvolver Int String
  | MakeDelay Int AudioParameter
  | MakeDynamicsCompressor Int AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter
  | MakeGain Int AudioParameter
  | MakeHighpass Int AudioParameter AudioParameter
  | MakeHighshelf Int AudioParameter AudioParameter
  | MakeLoopBuf Int String OnOff AudioParameter Number Number
  | MakeLowpass Int AudioParameter AudioParameter
  | MakeLowshelf Int AudioParameter AudioParameter
  | MakeMicrophone Int
  | MakeNotch Int AudioParameter AudioParameter
  | MakePeaking Int AudioParameter AudioParameter AudioParameter
  | MakePeriodicOsc Int String OnOff AudioParameter
  | MakePlayBuf Int String Number OnOff AudioParameter
  | MakeRecorder Int String
  | MakeSawtoothOsc Int OnOff AudioParameter
  | MakeSinOsc Int OnOff AudioParameter
  | MakeSquareOsc Int OnOff AudioParameter
  | MakeSpeaker Int
  | MakeStereoPanner Int AudioParameter
  | MakeTriangleOsc Int OnOff AudioParameter
  | MakeWaveShaper Int String Oversample
  | SetBuffer Int String
  | SetPeriodicOsc Int String
  | SetOn Int
  | SetOff Int
  | SetLoopStart Int Number
  | SetLoopEnd Int Number
  | SetRatio Int AudioParameter
  | SetOffset Int AudioParameter
  | SetAttack Int AudioParameter
  | SetGain Int AudioParameter
  | SetQ Int AudioParameter
  | SetPan Int AudioParameter
  | SetThreshold Int AudioParameter
  | SetRelease Int AudioParameter
  | SetKnee Int AudioParameter
  | SetDelay Int AudioParameter
  | SetPlaybackRate Int AudioParameter
  | SetFrequency Int AudioParameter

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
