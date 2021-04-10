module WAGS.Rendered where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import WAGS.Graph.Parameter (AudioParameter)

data Instruction
  = ConnectXToY Int Int
  | DisconnectXFromY Int Int
  | DestroyUnit Int
  | RebaseAllUnits (Array { from :: Int, to :: Int })
  | MakeAllpass Int AudioParameter AudioParameter
  | MakeBandpass Int AudioParameter AudioParameter
  | MakeConstant Int AudioParameter
  | MakeConvolver Int String
  | MakeDelay Int AudioParameter
  | MakeDynamicsCompressor Int AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter
  | MakeGain Int AudioParameter
  | MakeHighpass Int AudioParameter AudioParameter
  | MakeHighshelf Int AudioParameter AudioParameter
  | MakeLoopBuf Int String AudioParameter Number Number
  | MakeLowpass Int AudioParameter AudioParameter
  | MakeLowshelf Int AudioParameter AudioParameter
  | MakeMicrophone Int
  | MakeNotch Int AudioParameter AudioParameter
  | MakePeaking Int AudioParameter AudioParameter AudioParameter
  | MakePeriodicOsc Int String AudioParameter
  | MakePlayBuf Int String Number AudioParameter
  | MakeRecorder Int String
  | MakeSawtoothOsc Int AudioParameter
  | MakeSinOsc Int AudioParameter
  | MakeSquareOsc Int AudioParameter
  | MakeSpeaker Int
  | MakeStereoPanner Int AudioParameter
  | MakeTriangleOsc Int AudioParameter
  | MakeWaveShaper Int String Oversample
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

data Oversample
  = None
  | TwoX
  | FourX

derive instance eqOversample :: Eq Oversample

derive instance genericOversample :: Generic Oversample _

instance showOversample :: Show Oversample where
  show = genericShow

data AnAudioUnit
  = AAllpass AudioParameter AudioParameter
  | ABandpass AudioParameter AudioParameter
  | AConstant AudioParameter
  | AConvolver String
  | ADelay AudioParameter
  | ADynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter
  | AGain AudioParameter
  | AHighpass AudioParameter AudioParameter
  | AHighshelf AudioParameter AudioParameter
  | ALoopBuf String AudioParameter Number Number
  | ALowpass AudioParameter AudioParameter
  | ALowshelf AudioParameter AudioParameter
  | AMicrophone
  | ANotch AudioParameter AudioParameter
  | APeaking AudioParameter AudioParameter AudioParameter
  | APeriodicOsc String AudioParameter
  | APlayBuf String Number AudioParameter
  | ARecorder String
  | ASawtoothOsc AudioParameter
  | ASinOsc AudioParameter
  | ASpeaker
  | ASquareOsc AudioParameter
  | AStereoPanner AudioParameter
  | ATriangleOsc AudioParameter
  | AWaveShaper String Oversample

derive instance eqAnAudioUnit :: Eq AnAudioUnit

derive instance genericAnAudioUnit :: Generic AnAudioUnit _

instance showAnAudioUnit :: Show AnAudioUnit where
  show = genericShow
