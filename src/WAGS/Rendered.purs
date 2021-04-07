module WAGS.Rendered where

import Prelude
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.List ((:))
import Data.List as L
import Data.Show.Generic (genericShow)
import WAGS.Graph.Parameter (AudioParameter, AudioParameterTransition)

data Instruction
  = Stop Int
  | Free Int
  | DisconnectXFromY Int Int -- id id
  | ConnectXToY Int Int
  | NewUnit Int String
  | Rebase (Array { from :: Int, to :: Int })
  | SetFrequency Int Number Number AudioParameterTransition -- frequency
  | SetThreshold Int Number Number AudioParameterTransition -- threshold
  | SetKnee Int Number Number AudioParameterTransition -- knee
  | SetRatio Int Number Number AudioParameterTransition -- ratio
  | SetAttack Int Number Number AudioParameterTransition -- attack
  | SetRelease Int Number Number AudioParameterTransition -- release
  | SetBuffer Int Int (Array (Array Number)) -- buffer
  | SetQ Int Number Number AudioParameterTransition -- q
  | SetPlaybackRate Int Number Number AudioParameterTransition -- playback rate
  | SetPeriodicWave Int (Array Number) (Array Number) -- periodic wave
  | SetCurve Int (Array Number) -- curve
  | SetOversample Int String -- oversample
  | SetLoopStart Int Number Boolean -- loop start
  | SetLoopEnd Int Number Boolean -- loop end
  | SetPan Int Number Number AudioParameterTransition -- pan for pan node
  | SetGain Int Number Number AudioParameterTransition -- gain for gain node, boolean if is start
  | SetDelay Int Number Number AudioParameterTransition -- delay for delay node
  | SetOffset Int Number Number AudioParameterTransition -- offset for const node
  | SetCustomParam Int String Number Number AudioParameterTransition -- for audio worklet nodes
  | SetConeInnerAngle Int Number
  | SetConeOuterAngle Int Number
  | SetConeOuterGain Int Number
  | SetDistanceModel Int String
  | SetMaxDistance Int Number
  | SetOrientationX Int Number Number AudioParameterTransition
  | SetOrientationY Int Number Number AudioParameterTransition
  | SetOrientationZ Int Number Number AudioParameterTransition
  | SetPanningModel Int String
  | SetPositionX Int Number Number AudioParameterTransition
  | SetPositionY Int Number Number AudioParameterTransition
  | SetPositionZ Int Number Number AudioParameterTransition
  | SetRefDistance Int Number
  | SetRolloffFactor Int Number

derive instance eqInstruction :: Eq Instruction

derive instance genericInstruction :: Generic Instruction _

instance showInstruction :: Show Instruction where
  show = genericShow

instance ordInstruction :: Ord Instruction where
  compare (Stop x) (Stop y) = compare x y
  compare (Stop _) _ = LT
  compare (DisconnectXFromY x _) (DisconnectXFromY y _) = compare x y
  compare (DisconnectXFromY _ _) _ = LT
  compare (Free x) (Free y) = compare x y
  compare (Free _) _ = LT
  compare _ (Stop _) = GT
  compare _ (DisconnectXFromY _ _) = GT
  compare _ (Free _) = GT
  compare (ConnectXToY x _) (ConnectXToY y _) = compare x y
  compare (ConnectXToY _ _) _ = GT
  compare (NewUnit x _) (NewUnit y _) = compare x y
  compare (NewUnit _ _) _ = GT
  compare _ (ConnectXToY _ _) = LT
  compare _ (NewUnit _ _) = LT
  compare _ _ = EQ

sortInstructions :: Array Instruction -> Array Instruction
sortInstructions = A.fromFoldable <<< go mempty <<< L.fromFoldable
  where
  go l (Rebase x : b) = (L.sort l) <> pure (Rebase x) <> go mempty b

  go l (a : b) = go (a : l) b

  go l L.Nil = (L.sort l)

data AnAudioUnit
  = AAllpass AudioParameter AudioParameter
  | ABandpass AudioParameter AudioParameter
  | AConstant AudioParameter
  | AConvolver
  | ADelay AudioParameter
  | ADynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter
  | AGain AudioParameter
  | AHighpass AudioParameter AudioParameter
  | AHighshelf AudioParameter AudioParameter
  | ALoopBuf AudioParameter
  | ALowpass AudioParameter AudioParameter
  | ALowshelf AudioParameter AudioParameter
  | AMicrophone
  | ANotch AudioParameter AudioParameter
  | APeaking AudioParameter AudioParameter AudioParameter
  | APeriodicOsc AudioParameter
  | APlayBuf AudioParameter
  | ARecorder
  | ASawtoothOsc AudioParameter
  | ASinOsc AudioParameter
  | ASpeaker
  | ASquareOsc AudioParameter
  | AStereoPanner AudioParameter
  | ATriangleOsc AudioParameter
  | AWaveShaper

derive instance eqAnAudioUnit :: Eq AnAudioUnit

derive instance genericAnAudioUnit :: Generic AnAudioUnit _

instance showAnAudioUnit :: Show AnAudioUnit where
  show = genericShow
