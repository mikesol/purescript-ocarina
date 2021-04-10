module WAGS.Graph.Constructors where

import Type.Proxy (Proxy)


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
  = Constant a

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
  = LoopBuf (Proxy s) a Number Number

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
  = PeriodicOsc (Proxy s) a

data PlayBuf (s :: Symbol) a
  = PlayBuf (Proxy s) Number a

data Recorder (s :: Symbol) a
  = Recorder (Proxy s) a

data SawtoothOsc a
  = SawtoothOsc a

data SinOsc a
  = SinOsc a

data Speaker a
  = Speaker a

data SquareOsc a
  = SquareOsc a

data StereoPanner a b
  = StereoPanner a b

data TriangleOsc a
  = TriangleOsc a

data WaveShaper (s :: Symbol) a b
  = WaveShaper (Proxy s) a b
