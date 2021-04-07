module WAGS.Graph.Constructors where



import Type.Proxy (Proxy)

data OversampleNone = OversampleNone
data OversampleTwoX = OversampleTwoX
data OversampleFourX = OversampleFourX

data WaveShaper (s :: Symbol) a b
  = WaveShaper (Proxy s) a b

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

data Microphone (s :: Symbol)
  = Microphone (Proxy s)

data PlayBuf (s :: Symbol) a
  = PlayBuf (Proxy s) Number a

data LoopBuf (s :: Symbol) a
  = LoopBuf (Proxy s) a Number Number

data Convolver (s :: Symbol) a
  = Convolver (Proxy s) a

data DynamicsCompressor a b c d e f
  = DynamicsCompressor a b c d e f

data StereoPanner a b
  = StereoPanner a b

data Constant a
  = Constant a

data Delay a b
  = Delay a b

data Recorder (s :: Symbol) a
  = Recorder (Proxy s) a