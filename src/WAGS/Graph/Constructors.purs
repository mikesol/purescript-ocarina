module WAGS.Graph.Constructors where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy)

data OnOff
  = On
  | Off

derive instance eqOnOff :: Eq OnOff
derive instance genericOnOff :: Generic OnOff _
instance showOnOff :: Show OnOff where
  show = genericShow

-- for waveshaper
data OversampleNone
  = OversampleNone

data OversampleTwoX
  = OversampleTwoX

data OversampleFourX
  = OversampleFourX

--
data Allpass a b c
  = Allpass a b c

data Bandpass a b c
  = Bandpass a b c

data Constant a
  = Constant OnOff a

data Convolver (s :: Symbol) b
  = Convolver (Proxy s) b

data Delay a b
  = Delay a b

data Dup a b
  = Dup a b

data DynamicsCompressor a b c d e f
  = DynamicsCompressor a b c d e f

data Gain a b
  = Gain a b

data Highpass a b c
  = Highpass a b c

data Highshelf a b c
  = Highshelf a b c

data LoopBuf (s :: Symbol) a
  = LoopBuf (Proxy s) OnOff a Number Number

data Lowpass a b c
  = Lowpass a b c

data Lowshelf a b c
  = Lowshelf a b c

data Microphone
  = Microphone

data Notch a b c
  = Notch a b c

data Peaking a b c d
  = Peaking a b c d

data PeriodicOsc (s :: Symbol) a
  = PeriodicOsc (Proxy s) OnOff a

data PlayBuf (s :: Symbol) a
  = PlayBuf (Proxy s) Number OnOff a

data Recorder (s :: Symbol) a
  = Recorder (Proxy s) a

data SawtoothOsc a
  = SawtoothOsc OnOff a

data SinOsc a
  = SinOsc OnOff a

data Speaker a
  = Speaker a

data SquareOsc a
  = SquareOsc OnOff a

data StereoPanner a b
  = StereoPanner a b

data TriangleOsc a
  = TriangleOsc OnOff a

data WaveShaper (s :: Symbol) a b
  = WaveShaper (Proxy s) a b
