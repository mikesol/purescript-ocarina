module WAGS.Graph.IsAudio where

import Prelude

import Data.Identity (Identity)
import Data.Tuple (Tuple)
import Type.Proxy (Proxy)
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Decorators (Focus)
import WAGS.Rendered (Oversample(..))

class IsAudio (audio :: Type)

instance isAudioAllpass :: IsAudio (CTOR.Allpass a b c)

instance isAudioBandpass :: IsAudio (CTOR.Bandpass a b c)

instance isAudioConstant :: IsAudio (CTOR.Constant a)

instance isAudioConvolver :: IsAudio (CTOR.Convolver a b)

instance isAudioDelay :: IsAudio (CTOR.Delay a b)

instance isAudioDup :: IsAudio (CTOR.Dup a b)

instance isAudioDynamicsCompressor :: IsAudio (CTOR.DynamicsCompressor a b c d e f)

instance isAudioGain :: IsAudio (CTOR.Gain a b)

instance isAudioHighpass :: IsAudio (CTOR.Highpass a b c)

instance isAudioHighshelf :: IsAudio (CTOR.Highshelf a b c)

instance isAudioLoopBuf :: IsAudio (CTOR.LoopBuf a b)

instance isAudioLowpass :: IsAudio (CTOR.Lowpass a b c)

instance isAudioLowshelf :: IsAudio (CTOR.Lowshelf a b c)

instance isAudioMicrophone :: IsAudio CTOR.Microphone

instance isAudioNotch :: IsAudio (CTOR.Notch a b c)

instance isAudioPeaking :: IsAudio (CTOR.Peaking a b c b)

instance isAudioPeriodicOsc :: IsAudio (CTOR.PeriodicOsc a b)

instance isAudioPlayBuf :: IsAudio (CTOR.PlayBuf a b)

instance isAudioRecorder :: IsAudio (CTOR.Recorder a b)

instance isAudioSawtoothOsc :: IsAudio (CTOR.SawtoothOsc a)

instance isAudioSinOsc :: IsAudio (CTOR.SinOsc a)

instance isAudioSpeaker :: IsAudio (CTOR.Speaker a)

instance isAudioSquareOsc :: IsAudio (CTOR.SquareOsc a)

instance isAudioStereoPanner :: IsAudio (CTOR.StereoPanner a b)

instance isAudioTriangleOsc :: IsAudio (CTOR.TriangleOsc a)

instance isAudioWaveShaper :: IsAudio (CTOR.WaveShaper a b c)

instance isAudioProxy :: IsAudio (Proxy s)

instance isAudioIdentity :: IsAudio i => IsAudio (Identity i)

instance isAudioFocus :: IsAudio f => IsAudio (Focus f)

class IsMultiAudio (audio :: Type)

instance isMultiAudioUnit :: IsMultiAudio Unit
else instance isMultiAudioTuple :: (IsMultiAudio a, IsMultiAudio b) => IsMultiAudio (Tuple a b)
else instance isMultiAudioAudio :: IsAudio a => IsMultiAudio a

class IsAudioOrF (audioOrF :: Type)

instance isAudioOrFProxy :: IsAudio a => IsAudioOrF (Proxy s -> a)
else instance isAudioOrFAudio :: IsAudio a => IsAudioOrF a

class IsMultiAudioOrF (audioOrF :: Type)

instance isMultiAudioOrFProxy :: IsMultiAudio a => IsMultiAudioOrF (Proxy s -> a)
else instance isMultiAudioOrFAudio :: IsMultiAudio a => IsMultiAudioOrF a

class IsOversample oversample where
  reflectOversample :: oversample -> Oversample

instance isOversampleNone :: IsOversample CTOR.OversampleNone where
  reflectOversample _ = None 

instance isOversampleTwoX :: IsOversample CTOR.OversampleTwoX where
  reflectOversample _ = TwoX 

instance isOversampleFourX :: IsOversample CTOR.OversampleFourX where
  reflectOversample _ = FourX 