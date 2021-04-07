module WAGS.Graph.Constructors where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data Oversample
  = None
  | TwoX
  | FourX

derive instance genericOversample :: Generic Oversample _

instance showOversample :: Show Oversample where
  show s = genericShow s

derive instance eqOversample :: Eq Oversample

data WaveShaper a
  = WaveShaper String Oversample a

data SinOsc a
  = SinOsc a

data SawtoothOsc a
  = SawtoothOsc a

data TriangleOsc a
  = TriangleOsc a

data PeriodicOsc a
  = PeriodicOsc a

data SquareOsc a
  = SquareOsc a

data Dup a b
  = Dup a b

data Lowpass a b c
  = Lowpass a b c

data Highpass a b c
  = Highpass a b c

data Lowshelf a b c
  = Lowshelf a b c

data Highshelf a b c
  = Highshelf a b c

data Bandpass a b c
  = Bandpass a b c

data Peaking a b c d
  = Peaking a b c d

data Notch a b c
  = Notch a b c

data Allpass a b c
  = Allpass a b c

data Gain a b
  = Gain a b

data Speaker a
  = Speaker a

data Microphone
  = Microphone String

data PlayBuf a
  = PlayBuf String (Maybe Number) a

data LoopBuf a
  = LoopBuf String a Number Number

data Convolver a
  = Convolver String a

data DynamicsCompressor a b c d e f
  = DynamicsCompressor a b c d e f

data StereoPanner a b
  = StereoPanner a b

data Constant a
  = Constant a

data Delay a b
  = Delay a b

data Recorder a
  = Recorder String a
