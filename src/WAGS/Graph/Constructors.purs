module WAGS.Graph.Constructors where

import Prelude

import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


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

class IsAudio (audio :: Type)

instance isAudioAllpass :: IsAudio (Allpass a b c)

instance isAudioBandpass :: IsAudio (Bandpass a b c)

instance isAudioConstant :: IsAudio (Constant a)

instance isAudioConvolver :: IsAudio (Convolver a b)

instance isAudioDelay :: IsAudio (Delay a b)

instance isAudioDup :: IsAudio (Dup a b)

instance isAudioDynamicsCompressor :: IsAudio (DynamicsCompressor a b c d e f)

instance isAudioGain :: IsAudio (Gain a b)

instance isAudioHighpass :: IsAudio (Highpass a b c)

instance isAudioHighshelf :: IsAudio (Highshelf a b c)

instance isAudioLoopBuf :: IsAudio (LoopBuf a b)

instance isAudioLowpass :: IsAudio (Lowpass a b c)

instance isAudioLowshelf :: IsAudio (Lowshelf a b c)

instance isAudioMicrophone :: IsAudio Microphone

instance isAudioNotch :: IsAudio (Notch a b c)

instance isAudioPeaking :: IsAudio (Peaking a b c b)

instance isAudioPeriodicOsc :: IsAudio (PeriodicOsc a b)

instance isAudioPlayBuf :: IsAudio (PlayBuf a b)

instance isAudioRecorder :: IsAudio (Recorder a b)

instance isAudioSawtoothOsc :: IsAudio (SawtoothOsc a)

instance isAudioSinOsc :: IsAudio (SinOsc a)

instance isAudioSpeaker :: IsAudio (Speaker a)

instance isAudioSquareOsc :: IsAudio (SquareOsc a)

instance isAudioStereoPanner :: IsAudio (StereoPanner a b)

instance isAudioTriangleOsc :: IsAudio (TriangleOsc a)

instance isAudioWaveShaper :: IsAudio (WaveShaper a b c)
instance isAudioUnit :: IsAudio Unit
instance isAudioTuple :: (IsAudio a, IsAudio b) => IsAudio (Tuple a b)
instance isAudioProxy :: IsAudio (Proxy s)

class IsAudioOrF (audioOrF :: Type) (s :: Type) (audio :: Type) | audioOrF s -> audio where
  toF :: audioOrF -> (Proxy s -> audio)

instance isAudioOrFProxy :: IsAudio a => IsAudioOrF (Proxy s -> a) s a where
  toF f s = f s
else instance isAudioOrFAudio :: IsAudio a => IsAudioOrF a s a where
  toF = const