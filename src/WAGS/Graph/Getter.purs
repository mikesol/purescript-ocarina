module WAGS.Graph.Getter where

import Prelude
import Data.Identity (Identity)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Decorators (IgnoreMe, Focus, This)
import WAGS.Graph.Parameter (AudioParameter)

type A2A
  = AudioParameter -> AudioParameter

-- | Class that converts a graph to a getter. A getter replaces all setter functions
-- | with the `identity` function. Then, when passed to a cursor, it resolves with the current values.
-- | Note that getting is as expensive as setting. Instead of using a getter, consider using an accumulator or a setter, as setters have the previous value passed into them. However, for more complicated effects, getters can be a good option.
class AsGetter (a :: Type) (b :: Type) | a -> b where
  asGetter :: a -> b

instance asGetterAllpass :: AsGetter c c' => AsGetter (CTOR.Allpass a b c) (CTOR.Allpass A2A A2A c') where
  asGetter (CTOR.Allpass _ _ c) = CTOR.Allpass identity identity (asGetter c)

instance asGetterBandpass :: AsGetter c c' => AsGetter (CTOR.Bandpass a b c) (CTOR.Bandpass A2A A2A c') where
  asGetter (CTOR.Bandpass _ _ c) = CTOR.Bandpass identity identity (asGetter c)

instance asGetterConstant :: AsGetter (CTOR.Constant a) (CTOR.Constant A2A) where
  asGetter (CTOR.Constant a _) = CTOR.Constant a identity

instance asGetterConvolver :: AsGetter b b' => AsGetter (CTOR.Convolver a b) (CTOR.Convolver a b') where
  asGetter (CTOR.Convolver a b) = CTOR.Convolver a (asGetter b)

instance asGetterDelay :: AsGetter b b' => AsGetter (CTOR.Delay a b) (CTOR.Delay A2A b') where
  asGetter (CTOR.Delay _ b) = CTOR.Delay identity (asGetter b)

instance asGetterDup :: (AsGetter a a', AsGetter b b') => AsGetter (CTOR.Dup a b) (CTOR.Dup a' b') where
  asGetter (CTOR.Dup a b) = CTOR.Dup (asGetter a) (asGetter b)

instance asGetterDynamicsCompressor :: AsGetter f f' => AsGetter (CTOR.DynamicsCompressor a b c d e f) (CTOR.DynamicsCompressor A2A A2A A2A A2A A2A f') where
  asGetter (CTOR.DynamicsCompressor _ _ _ _ _ f) = CTOR.DynamicsCompressor identity identity identity identity identity (asGetter f)

instance asGetterGain :: AsGetter b b' => AsGetter (CTOR.Gain a b) (CTOR.Gain A2A b') where
  asGetter (CTOR.Gain _ b) = CTOR.Gain identity (asGetter b)

instance asGetterHighpass :: AsGetter c c' => AsGetter (CTOR.Highpass a b c) (CTOR.Highpass A2A A2A c') where
  asGetter (CTOR.Highpass _ _ c) = CTOR.Highpass identity identity (asGetter c)

instance asGetterHighshelf :: AsGetter c c' => AsGetter (CTOR.Highshelf a b c) (CTOR.Highshelf A2A A2A c') where
  asGetter (CTOR.Highshelf _ _ c) = CTOR.Highshelf identity identity (asGetter c)

instance asGetterLoopBuf :: AsGetter (CTOR.LoopBuf a) (CTOR.LoopBuf A2A) where
  asGetter (CTOR.LoopBuf a b _ d e) = CTOR.LoopBuf a b identity d e

instance asGetterLowpass :: AsGetter c c' => AsGetter (CTOR.Lowpass a b c) (CTOR.Lowpass A2A A2A c') where
  asGetter (CTOR.Lowpass _ _ c) = CTOR.Lowpass identity identity (asGetter c)

instance asGetterLowshelf :: AsGetter c c' => AsGetter (CTOR.Lowshelf a b c) (CTOR.Lowshelf A2A A2A c') where
  asGetter (CTOR.Lowshelf _ _ c) = CTOR.Lowshelf identity identity (asGetter c)

instance asGetterMicrophone :: AsGetter CTOR.Microphone CTOR.Microphone where
  asGetter = identity

instance asGetterNotch :: AsGetter c c' => AsGetter (CTOR.Notch a b c) (CTOR.Notch A2A A2A c') where
  asGetter (CTOR.Notch _ _ c) = CTOR.Notch identity identity (asGetter c)

instance asGetterPeaking :: AsGetter d d' => AsGetter (CTOR.Peaking a b c d) (CTOR.Peaking A2A A2A A2A d') where
  asGetter (CTOR.Peaking _ _ _ c) = CTOR.Peaking identity identity identity (asGetter c)

instance asGetterPeriodicOsc :: AsGetter (CTOR.PeriodicOsc a) (CTOR.PeriodicOsc A2A) where
  asGetter (CTOR.PeriodicOsc a b _) = CTOR.PeriodicOsc a b identity

instance asGetterPlayBuf :: AsGetter (CTOR.PlayBuf a) (CTOR.PlayBuf A2A) where
  asGetter (CTOR.PlayBuf a b c _) = CTOR.PlayBuf a b c identity

instance asGetterRecorder :: AsGetter b b' => AsGetter (CTOR.Recorder a b) (CTOR.Recorder a b') where
  asGetter (CTOR.Recorder a b) = CTOR.Recorder a (asGetter b)

instance asGetterSawtoothOsc :: AsGetter (CTOR.SawtoothOsc a) (CTOR.SawtoothOsc A2A) where
  asGetter (CTOR.SawtoothOsc a _) = CTOR.SawtoothOsc a identity

instance asGetterSinOsc :: AsGetter (CTOR.SinOsc a) (CTOR.SinOsc A2A) where
  asGetter (CTOR.SinOsc a _) = CTOR.SinOsc a identity

instance asGetterSpeaker :: AsGetter a a' => AsGetter (CTOR.Speaker a) (CTOR.Speaker a') where
  asGetter (CTOR.Speaker a) = CTOR.Speaker (asGetter a)

instance asGetterSquareOsc :: AsGetter (CTOR.SquareOsc a) (CTOR.SquareOsc A2A) where
  asGetter (CTOR.SquareOsc a _) = CTOR.SquareOsc a identity

instance asGetterStereoPanner :: AsGetter b b' => AsGetter (CTOR.StereoPanner a b) (CTOR.StereoPanner A2A b') where
  asGetter (CTOR.StereoPanner _ b) = CTOR.StereoPanner identity (asGetter b)

instance asGetterTriangleOsc :: AsGetter (CTOR.TriangleOsc a) (CTOR.TriangleOsc A2A) where
  asGetter (CTOR.TriangleOsc a _) = CTOR.TriangleOsc a identity

instance asGetterWaveShaper :: AsGetter c c' => AsGetter (CTOR.WaveShaper a b c) (CTOR.WaveShaper a b c') where
  asGetter (CTOR.WaveShaper a b c) = CTOR.WaveShaper a b (asGetter c)

instance asGetterProxy :: AsGetter (Proxy s) (Proxy s) where
  asGetter = identity

instance asGetterThis :: AsGetter This This where
  asGetter = identity

instance asGetterIgnoreMe :: AsGetter IgnoreMe IgnoreMe where
  asGetter = identity

instance asGetterFocus :: AsGetter i i' => AsGetter (Focus i) (Focus i') where
  asGetter = map asGetter

instance asGetterIdntity :: AsGetter i i' => AsGetter (Identity i) (Identity i') where
  asGetter = map asGetter

instance asGetterUnit :: AsGetter Unit Unit where
  asGetter = identity

instance isMultiAudioTuple :: (AsGetter a a', AsGetter b b') => AsGetter (Tuple a b) (Tuple a' b') where
  asGetter (Tuple a b) = Tuple (asGetter a) (asGetter b)

instance asGetterOrFProxy :: AsGetter a a' => AsGetter (Proxy s -> a) (Proxy s -> a') where
  asGetter f = pure (asGetter (f Proxy))
