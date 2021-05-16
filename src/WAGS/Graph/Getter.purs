module WAGS.Graph.Getter where

import Prelude
import Data.Symbol (class IsSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import Prim.Row as R
import Record as Record
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Parameter (AudioParameter)

type A2A
  = AudioParameter -> AudioParameter

-- | Class that converts a graph to a getter. A getter replaces all setter functions
-- | with the `identity` function. Then, when passed to a cursor, it resolves with the current values.
-- | Note that getting is as expensive as setting. Instead of using a getter, consider using an accumulator or a setter, as setters have the previous value passed into them. However, for more complicated effects, getters can be a good option.
class AsGetter (a :: Type) (b :: Type) | a -> b where
  asGetter :: a -> b

instance asGetterAllpass :: AsGetter (CTOR.Allpass a b) (CTOR.Allpass A2A A2A) where
  asGetter (CTOR.Allpass _ _) = CTOR.Allpass identity identity

instance asGetterBandpass :: AsGetter (CTOR.Bandpass a b) (CTOR.Bandpass A2A A2A) where
  asGetter (CTOR.Bandpass _ _) = CTOR.Bandpass identity identity

instance asGetterConstant :: AsGetter (CTOR.Constant a) (CTOR.Constant A2A) where
  asGetter (CTOR.Constant a _) = CTOR.Constant a identity

instance asGetterConvolver :: AsGetter (CTOR.Convolver a) (CTOR.Convolver a) where
  asGetter = identity

instance asGetterDelay :: AsGetter (CTOR.Delay a) (CTOR.Delay A2A) where
  asGetter (CTOR.Delay _) = CTOR.Delay identity

instance asGetterDynamicsCompressor :: AsGetter f f' => AsGetter (CTOR.DynamicsCompressor a b c d e) (CTOR.DynamicsCompressor A2A A2A A2A A2A A2A) where
  asGetter (CTOR.DynamicsCompressor _ _ _ _ _) = CTOR.DynamicsCompressor identity identity identity identity identity

instance asGetterGain :: AsGetter (CTOR.Gain a) (CTOR.Gain A2A) where
  asGetter (CTOR.Gain _) = CTOR.Gain identity

instance asGetterHighpass :: AsGetter (CTOR.Highpass a b) (CTOR.Highpass A2A A2A) where
  asGetter (CTOR.Highpass _ _) = CTOR.Highpass identity identity

instance asGetterHighshelf :: AsGetter (CTOR.Highshelf a b) (CTOR.Highshelf A2A A2A) where
  asGetter (CTOR.Highshelf _ _) = CTOR.Highshelf identity identity

instance asGetterLoopBuf :: AsGetter (CTOR.LoopBuf a) (CTOR.LoopBuf A2A) where
  asGetter (CTOR.LoopBuf a b _ d e) = CTOR.LoopBuf a b identity d e

instance asGetterLowpass :: AsGetter (CTOR.Lowpass a b) (CTOR.Lowpass A2A A2A) where
  asGetter (CTOR.Lowpass _ _) = CTOR.Lowpass identity identity

instance asGetterLowshelf :: AsGetter (CTOR.Lowshelf a b) (CTOR.Lowshelf A2A A2A) where
  asGetter (CTOR.Lowshelf _ _) = CTOR.Lowshelf identity identity

instance asGetterMicrophone :: AsGetter CTOR.Microphone CTOR.Microphone where
  asGetter = identity

instance asGetterNotch :: AsGetter (CTOR.Notch a b) (CTOR.Notch A2A A2A) where
  asGetter (CTOR.Notch _ _) = CTOR.Notch identity identity

instance asGetterPeaking :: AsGetter d d' => AsGetter (CTOR.Peaking a b c) (CTOR.Peaking A2A A2A A2A) where
  asGetter (CTOR.Peaking _ _ _) = CTOR.Peaking identity identity identity

instance asGetterPeriodicOsc :: AsGetter (CTOR.PeriodicOsc a) (CTOR.PeriodicOsc A2A) where
  asGetter (CTOR.PeriodicOsc a b _) = CTOR.PeriodicOsc a b identity

instance asGetterPlayBuf :: AsGetter (CTOR.PlayBuf a) (CTOR.PlayBuf A2A) where
  asGetter (CTOR.PlayBuf a b c _) = CTOR.PlayBuf a b c identity

instance asGetterRecorder :: AsGetter (CTOR.Recorder a) (CTOR.Recorder a) where
  asGetter = identity

instance asGetterSawtoothOsc :: AsGetter (CTOR.SawtoothOsc a) (CTOR.SawtoothOsc A2A) where
  asGetter (CTOR.SawtoothOsc a _) = CTOR.SawtoothOsc a identity

instance asGetterSinOsc :: AsGetter (CTOR.SinOsc a) (CTOR.SinOsc A2A) where
  asGetter (CTOR.SinOsc a _) = CTOR.SinOsc a identity

instance asGetterSpeaker :: AsGetter a a' => AsGetter (CTOR.Speaker) (CTOR.Speaker) where
  asGetter = identity

instance asGetterSquareOsc :: AsGetter (CTOR.SquareOsc a) (CTOR.SquareOsc A2A) where
  asGetter (CTOR.SquareOsc a _) = CTOR.SquareOsc a identity

instance asGetterStereoPanner :: AsGetter (CTOR.StereoPanner a) (CTOR.StereoPanner A2A) where
  asGetter (CTOR.StereoPanner _) = CTOR.StereoPanner identity

instance asGetterTriangleOsc :: AsGetter (CTOR.TriangleOsc a) (CTOR.TriangleOsc A2A) where
  asGetter (CTOR.TriangleOsc a _) = CTOR.TriangleOsc a identity

instance asGetterWaveShaper :: AsGetter (CTOR.WaveShaper a b) (CTOR.WaveShaper a b) where
  asGetter = identity

data AsGetterFoldingWithIndex
  = AsGetterFoldingWithIndex

instance getterFoldingWithIndex ::
  ( AsGetter node outNode
  , IsSymbol sym
  , R.Lacks sym inRecord
  , R.Cons sym outNode inRecord outRecord
  ) =>
  FoldingWithIndex
    AsGetterFoldingWithIndex
    (proxy sym)
    { | inRecord }
    node
    { | outRecord } where
  foldingWithIndex AsGetterFoldingWithIndex prop ir node =
    Record.insert
      prop
      (asGetter node)
      ir
