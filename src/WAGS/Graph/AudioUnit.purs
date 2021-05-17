module WAGS.Graph.AudioUnit where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as R
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Edgeable (class Edgeable, withEdge)
import WAGS.Graph.Parameter (AudioParameter)

-- | Term-level constructor for an allpass filter.
-- | - `frequency` - the frequency where the phase transition occurs.
-- | - `q` - the width of the filter.
data Allpass frequency q
  = Allpass frequency q

type GetAllpass
  = Allpass AudioParameter AudioParameter

-- | Term-level constructor for a bandpass filter.
-- | - `frequency` - the frequency of the isolated band.
-- | - `q` - the width of the filter.
data Bandpass frequency q
  = Bandpass frequency q

type GetBandpass
  = Bandpass AudioParameter AudioParameter

-- | Term-level constructor for a constant value, aka DC offset.
-- | - `OnOff` - whether the generator is on or off.
-- | - `offset` - the amount of DC offset.
data Constant offset
  = Constant OnOff offset

type GetConstant
  = Constant AudioParameter

-- | Term-level constructor for a convolver, aka reverb.
-- | - `buffer` - the buffer of the impulse response of the space.
data Convolver (buffer :: Symbol)
  = Convolver (Proxy buffer)

type GetConvolver buffer
  = Convolver buffer

-- | Term-level constructor for a delay unit.
-- | - `delay` - the delay to apply.
data Delay delay
  = Delay delay

type GetDelay
  = Delay AudioParameter

-- | Term-level constructor for a compressor.
-- | - `threshold` - The threshold under which compression kicks in.
-- | - `knee` - The kink of the compression.
-- | - `ratio` - The amount of compression to apply.
-- | - `attack` - How far we look ahead. Longer attacks will lead to more crisp compression at the expense of an audible delay.
-- | - `release` - How long the release time of compression should be.
data DynamicsCompressor threshold knee ratio attack release
  = DynamicsCompressor threshold knee ratio attack release

type GetDynamicsCompressor
  = DynamicsCompressor
      AudioParameter
      AudioParameter
      AudioParameter
      AudioParameter
      AudioParameter

-- | Term-level constructor for a gain unit.
-- | - `volume` - the volume of the gain from 0 to 1.
data Gain volume
  = Gain volume

type GetGain
  = Gain AudioParameter

-- | Term-level constructor for a highpass filter.
-- | - `frequency` - the frequency below which we start to filter.
-- | - `q` - the width of the filter.
data Highpass frequency q
  = Highpass frequency q

type GetHighpass
  = Highpass AudioParameter AudioParameter

-- | Term-level constructor for a highshelf filter.
-- | - `frequency` - the frequency above which we start to filter.
-- | - `gain` - the boost or the amount of attenuation to apply.
data Highshelf frequency gain
  = Highshelf frequency gain

type GetHighshelf
  = Highshelf AudioParameter AudioParameter

-- | Term-level constructor for a looping buffer.
-- | - `buffer` - a string representing the buffer to use. Note that this string, when reset, will only reset the buffer when it is stopped.
-- | - `OnOff` - whether or not the generator is on or off.
-- | - `playbackRate` - the playback rate.
-- | - `Number` - where in the file the loop should start.
-- | - `Number` - where in the file the loop should end. A value of 0.0 or less means play to the end of the buffer.
data LoopBuf playbackRate
  = LoopBuf String OnOff playbackRate Number Number

type GetLoopBuf
  = LoopBuf AudioParameter

-- | Term-level constructor for a lowpass filter.
-- | - `frequency` - the frequency above which we start to filter.
-- | - `q` - the width of the filter.
data Lowpass frequency q
  = Lowpass frequency q

type GetLowpass
  = Lowpass AudioParameter AudioParameter

-- | Term-level constructor for a lowshelf filter.
-- | - `frequency` - the frequency below which we start to filter.
-- | - `q` - the width of the filter.
data Lowshelf frequency gain
  = Lowshelf frequency gain

type GetLowshelf
  = Lowshelf AudioParameter AudioParameter

-- | Term-level constructor for a microphone
data Microphone
  = Microphone

type GetMicrophone
  = Microphone

-- | Term-level constructor for a notch (aka band-reject) filter.
-- | - `frequency` - the frequency we are rejecting.
-- | - `q` - the width of the filter.
data Notch frequency q
  = Notch frequency q

type GetNotch
  = Notch AudioParameter AudioParameter

-- | Term-level constructor for a peaking filter. A peaking filter is a combination of bandpass and notch where the gain parameter modulates whether we are reinforcing or attenuating a frequency.
-- | - `frequency` - the frequency we are emphasizing _or_ rejecting.
-- | - `q` - the width of the filter.
-- | - `gain` - if positive, we are emphasizing the frequency. If negative, we are rejecting it.
data Peaking frequency q gain
  = Peaking frequency q gain

type GetPeaking
  = Peaking AudioParameter AudioParameter AudioParameter

-- | Term-level constructor for a periodic oscillator.
-- | - `periodicOsc` - the name of the wave table we'll be using. Note that, for a chance to take effect, the periodic oscillator must be stopped.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data PeriodicOsc frequency
  = PeriodicOsc String OnOff frequency

type GetPeriodicOsc
  = PeriodicOsc AudioParameter

-- | Term-level constructor for a playback buffer.
-- | - `buffer` - a string representing the buffer to use. Note that this string, when reset, will only reset the buffer when it is stopped.
-- | - `Number` - where in the file the playback should start.
-- | - `OnOff` - whether or not the generator is on or off.
-- | - `playbackRate` - the playback rate.
data PlayBuf playbackRate
  = PlayBuf String Number OnOff playbackRate

type GetPlayBuf
  = PlayBuf AudioParameter

-- | Term-level constructor for a recorder.
-- | - `recorder` - the recorder to which we write data.
data Recorder (recorder :: Symbol)
  = Recorder (Proxy recorder)

type GetRecorder recorder
  = Recorder recorder

-- | Term-level constructor for a sawtooth oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data SawtoothOsc frequency
  = SawtoothOsc OnOff frequency

type GetSawtoothOsc
  = SawtoothOsc AudioParameter

-- | Term-level constructor for a sine-wave oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data SinOsc frequency
  = SinOsc OnOff frequency

type GetSinOsc
  = SinOsc AudioParameter

-- | Term-level constructor for a loudspeaker.
data Speaker
  = Speaker

type GetSpeaker
  = Speaker

-- | Term-level constructor for a square-wave oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data SquareOsc frequency
  = SquareOsc OnOff frequency

type GetSquareOsc
  = SquareOsc AudioParameter

-- | Term-level constructor for a stereo panner.
-- | - `pan` - the amount of pan to apply, where -1.0 is fully to the left and 1.0 is fully to the right.
data StereoPanner pan
  = StereoPanner pan

type GetStereoPanner
  = StereoPanner AudioParameter

-- | Term-level constructor for a triangle oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data TriangleOsc frequency
  = TriangleOsc OnOff frequency

type GetTriangleOsc
  = TriangleOsc AudioParameter

-- | Term-level constructor for a WaveShaper, aka distortion.
-- | - `floatArray` - the shape of the distortion.
-- | - `oversample` - how much to oversample - none, 2x or 4x. Once set, this cannot change without destroying and remaking the audio unit.
data WaveShaper (floatArray :: Symbol) oversample
  = WaveShaper (Proxy floatArray) oversample

type GetWaveShaper floatArray oversample
  = WaveShaper floatArray oversample

-- | Term-level constructor for a generator being on or off
data OnOff
  = On
  | Off

derive instance eqOnOff :: Eq OnOff

derive instance genericOnOff :: Generic OnOff _

instance showOnOff :: Show OnOff where
  show = genericShow

-- | Type-level oversample none for a wave shaper. This is at the type-level and not the term-level via an ADT because we need make sure to construct an entirely new wave shaper if the value changes.
data OversampleNone
  = OversampleNone

instance semigroupOversampleNone :: Semigroup OversampleNone where
  append _ _ = OversampleNone

instance monoidOversampleNone :: Monoid OversampleNone where
  mempty = OversampleNone

-- | Type-level oversample 2x for a wave shaper. This is at the type-level and not the term-level via an ADT because we need make sure to construct an entirely new wave shaper if the value changes.
data OversampleTwoX
  = OversampleTwoX

instance semigroupOversampleTwoX :: Semigroup OversampleTwoX where
  append _ _ = OversampleTwoX

instance monoidOversampleTwoX :: Monoid OversampleTwoX where
  mempty = OversampleTwoX

-- | Type-level oversample 4x for a wave shaper. This is at the type-level and not the term-level via an ADT because we need make sure to construct an entirely new wave shaper if the value changes.
data OversampleFourX
  = OversampleFourX

instance semigroupOversampleFourX :: Semigroup OversampleFourX where
  append _ _ = OversampleFourX

instance monoidOversampleFourX :: Monoid OversampleFourX where
  mempty = OversampleFourX

class ReifyAU a b | a -> b where
  reifyAU :: a -> b

-- | Type-level constructor for an allpass filter.
data TAllpass
  = TAllpass

instance semigroupTAllpass :: Semigroup TAllpass where
  append _ _ = TAllpass

instance monoidTAllpass :: Monoid TAllpass where
  mempty = TAllpass

instance reifyTAllpass :: ReifyAU (Allpass a b) TAllpass where
  reifyAU = const mempty

-- | Type-level constructor for a bandpass filter.
data TBandpass
  = TBandpass

instance semigroupTBandpass :: Semigroup TBandpass where
  append _ _ = TBandpass

instance monoidTBandpass :: Monoid TBandpass where
  mempty = TBandpass

instance reifyTBandpass :: ReifyAU (Bandpass a b) TBandpass where
  reifyAU = const mempty

-- | Type-level constructor for a constant value.
data TConstant
  = TConstant

instance semigroupTConstant :: Semigroup TConstant where
  append _ _ = TConstant

instance monoidTConstant :: Monoid TConstant where
  mempty = TConstant

instance reifyTConstant :: ReifyAU (Constant a) TConstant where
  reifyAU = const mempty

-- | Type-level constructor for a convolver, aka reverb.
data TConvolver (sym :: Symbol)
  = TConvolver (Proxy sym)

instance semigroupTConvolver :: Semigroup (TConvolver sym) where
  append _ _ = TConvolver (Proxy :: _ sym)

instance monoidTConvolver :: Monoid (TConvolver sym) where
  mempty = TConvolver (Proxy :: _ sym)

instance reifyTConvolver :: ReifyAU (Convolver sym) (TConvolver sym) where
  reifyAU = const mempty

-- | Type-level constructor for a delay unit.
data TDelay
  = TDelay

instance semigroupTDelay :: Semigroup TDelay where
  append _ _ = TDelay

instance monoidTDelay :: Monoid TDelay where
  mempty = TDelay

instance reifyTDelay :: ReifyAU (Delay a) TDelay where
  reifyAU = const mempty

-- | Type-level constructor for a compressor.
data TDynamicsCompressor
  = TDynamicsCompressor

instance semigroupTDynamicsCompressor :: Semigroup TDynamicsCompressor where
  append _ _ = TDynamicsCompressor

instance monoidTDynamicsCompressor :: Monoid TDynamicsCompressor where
  mempty = TDynamicsCompressor

instance reifyTDynamicsCompressor :: ReifyAU (DynamicsCompressor a b c d e) TDynamicsCompressor where
  reifyAU = const mempty

-- | Type-level constructor for a gain unit.
data TGain
  = TGain

instance semigroupTGain :: Semigroup TGain where
  append _ _ = TGain

instance monoidTGain :: Monoid TGain where
  mempty = TGain

instance reifyTGain :: ReifyAU (Gain a) TGain where
  reifyAU = const mempty

-- | Type-level constructor for a highpass filter.
data THighpass
  = THighpass

instance semigroupTHighpass :: Semigroup THighpass where
  append _ _ = THighpass

instance monoidTHighpass :: Monoid THighpass where
  mempty = THighpass

instance reifyTHighpass :: ReifyAU (Highpass a b) THighpass where
  reifyAU = const mempty

-- | Type-level constructor for a highshelf filter.
data THighshelf
  = THighshelf

instance semigroupTHighshelf :: Semigroup THighshelf where
  append _ _ = THighshelf

instance monoidTHighshelf :: Monoid THighshelf where
  mempty = THighshelf

instance reifyTHighshelf :: ReifyAU (Highshelf a b) THighshelf where
  reifyAU = const mempty

-- | Type-level constructor for a looping buffer.
data TLoopBuf
  = TLoopBuf

instance semigroupTLoopBuf :: Semigroup TLoopBuf where
  append _ _ = TLoopBuf

instance monoidTLoopBuf :: Monoid TLoopBuf where
  mempty = TLoopBuf

instance reifyTLoopBuf :: ReifyAU (LoopBuf a) TLoopBuf where
  reifyAU = const mempty

-- | Type-level constructor for a lowpass filter.
data TLowpass
  = TLowpass

instance semigroupTLowpass :: Semigroup TLowpass where
  append _ _ = TLowpass

instance monoidTLowpass :: Monoid TLowpass where
  mempty = TLowpass

instance reifyTLowpass :: ReifyAU (Lowpass a b) TLowpass where
  reifyAU = const mempty

-- | Type-level constructor for a lowshelf filter.
data TLowshelf
  = TLowshelf

instance semigroupTLowshelf :: Semigroup TLowshelf where
  append _ _ = TLowshelf

instance monoidTLowshelf :: Monoid TLowshelf where
  mempty = TLowshelf

instance reifyTLowshelf :: ReifyAU (Lowshelf a b) TLowshelf where
  reifyAU = const mempty

-- | Type-level constructor for a microphone.
data TMicrophone
  = TMicrophone

instance semigroupTMicrophone :: Semigroup TMicrophone where
  append _ _ = TMicrophone

instance monoidTMicrophone :: Monoid TMicrophone where
  mempty = TMicrophone

instance reifyTMicrophone :: ReifyAU Microphone TMicrophone where
  reifyAU = const mempty

-- | Type-level constructor for a notch filter.
data TNotch
  = TNotch

instance semigroupTNotch :: Semigroup TNotch where
  append _ _ = TNotch

instance monoidTNotch :: Monoid TNotch where
  mempty = TNotch

instance reifyTNotch :: ReifyAU (Notch a b) TNotch where
  reifyAU = const mempty

-- | Type-level constructor for a peaking filter.
data TPeaking
  = TPeaking

instance semigroupTPeaking :: Semigroup TPeaking where
  append _ _ = TPeaking

instance monoidTPeaking :: Monoid TPeaking where
  mempty = TPeaking

instance reifyTPeaking :: ReifyAU (Peaking a b c) TPeaking where
  reifyAU = const mempty

-- | Type-level constructor for a periodic oscillator.
data TPeriodicOsc
  = TPeriodicOsc

instance semigroupTPeriodicOsc :: Semigroup TPeriodicOsc where
  append _ _ = TPeriodicOsc

instance monoidTPeriodicOsc :: Monoid TPeriodicOsc where
  mempty = TPeriodicOsc

instance reifyTPeriodicOsc :: ReifyAU (PeriodicOsc a) TPeriodicOsc where
  reifyAU = const mempty

-- | Type-level constructor for playback from a buffer.
data TPlayBuf
  = TPlayBuf

instance semigroupTPlayBuf :: Semigroup TPlayBuf where
  append _ _ = TPlayBuf

instance monoidTPlayBuf :: Monoid TPlayBuf where
  mempty = TPlayBuf

instance reifyTPlayBuf :: ReifyAU (PlayBuf a) TPlayBuf where
  reifyAU = const mempty

-- | Type-level constructor for a recorder.
data TRecorder (sym :: Symbol)
  = TRecorder (Proxy sym)

instance semigroupTRecorder :: Semigroup (TRecorder sym) where
  append _ _ = TRecorder (Proxy :: _ sym)

instance monoidTRecorder :: Monoid (TRecorder sym) where
  mempty = TRecorder (Proxy :: _ sym)

instance reifyTRecorder :: ReifyAU (Recorder sym) (TRecorder sym) where
  reifyAU = const mempty

-- | Type-level constructor for a sawtooth oscillator.
data TSawtoothOsc
  = TSawtoothOsc

instance semigroupTSawtoothOsc :: Semigroup TSawtoothOsc where
  append _ _ = TSawtoothOsc

instance monoidTSawtoothOsc :: Monoid TSawtoothOsc where
  mempty = TSawtoothOsc

instance reifyTSawtoothOsc :: ReifyAU (SawtoothOsc a) TSawtoothOsc where
  reifyAU = const mempty

-- | Type-level constructor for a sine-wave oscillator.
data TSinOsc
  = TSinOsc

instance semigroupTSinOsc :: Semigroup TSinOsc where
  append _ _ = TSinOsc

instance monoidTSinOsc :: Monoid TSinOsc where
  mempty = TSinOsc

instance reifyTSinOsc :: ReifyAU (SinOsc a) TSinOsc where
  reifyAU = const mempty

-- | Type-level constructor for a loudspeaker.
data TSpeaker
  = TSpeaker

instance semigroupTSpeaker :: Semigroup TSpeaker where
  append _ _ = TSpeaker

instance monoidTSpeaker :: Monoid TSpeaker where
  mempty = TSpeaker

instance reifyTSpeaker :: ReifyAU Speaker TSpeaker where
  reifyAU = const mempty

-- | Type-level constructor for a square-wave oscillator.
data TSquareOsc
  = TSquareOsc

instance semigroupTSquareOsc :: Semigroup TSquareOsc where
  append _ _ = TSquareOsc

instance monoidTSquareOsc :: Monoid TSquareOsc where
  mempty = TSquareOsc

instance reifyTSquareOsc :: ReifyAU (SquareOsc a) TSquareOsc where
  reifyAU = const mempty

-- | Type-level constructor for a stereo panner.
data TStereoPanner
  = TStereoPanner

instance semigroupTStereoPanner :: Semigroup TStereoPanner where
  append _ _ = TStereoPanner

instance monoidTStereoPanner :: Monoid TStereoPanner where
  mempty = TStereoPanner

instance reifyTStereoPanner :: ReifyAU (StereoPanner a) TStereoPanner where
  reifyAU = const mempty

-- | Type-level constructor for a triangle oscillator.
data TTriangleOsc
  = TTriangleOsc

instance semigroupTTriangleOsc :: Semigroup TTriangleOsc where
  append _ _ = TTriangleOsc

instance monoidTTriangleOsc :: Monoid TTriangleOsc where
  mempty = TTriangleOsc

instance reifyTTriangleOsc :: ReifyAU (TriangleOsc a) TTriangleOsc where
  reifyAU = const mempty

-- | Type-level constructor for a wave shaper.
data TWaveShaper (sym :: Symbol) (oversample :: Type)
  = TWaveShaper (Proxy sym) oversample

instance semigroupTWaveShaper :: Monoid b => Semigroup (TWaveShaper a b) where
  append _ _ = TWaveShaper (Proxy :: _ a) mempty

instance monoidTWaveShaper :: Monoid b => Monoid (TWaveShaper a b) where
  mempty = TWaveShaper (Proxy :: _ a) mempty

instance reifyTWaveShaper :: Monoid b => ReifyAU (WaveShaper a b) (TWaveShaper a b) where
  reifyAU = const mempty

data Obliterate
  = Obliterate

instance mappingObliterate :: Mapping Obliterate a Unit where
  mapping Obliterate = const unit

data ReifyAUFoldingWithIndex
  = ReifyAUFoldingWithIndex

instance reifyAUFoldingWithIndexUnit ::
  FoldingWithIndex
    ReifyAUFoldingWithIndex
    (proxy sym)
    { | inRecord }
    Unit
    { | inRecord } where
  foldingWithIndex ReifyAUFoldingWithIndex _ i = const i
else instance reifyAUFoldingWithIndex ::
  ( Edgeable node' (node /\ edges)
  , ReifyAU node asType
  , IsSymbol sym
  , R.Lacks sym inRecord
  , HMap Obliterate edges obliterated
  , R.Cons sym (asType /\ obliterated) inRecord midRecord
  , HFoldlWithIndex
      ReifyAUFoldingWithIndex
      { | midRecord }
      edges
      { | outRecord }
  ) =>
  FoldingWithIndex
    ReifyAUFoldingWithIndex
    (proxy sym)
    { | inRecord }
    node'
    { | outRecord } where
  foldingWithIndex ReifyAUFoldingWithIndex prop ir node' = out
    where
    node /\ edges = withEdge node'

    au = reifyAU node

    edg = hmap Obliterate edges

    out = hfoldlWithIndex ReifyAUFoldingWithIndex (Record.insert prop (au /\ edg) ir) edges

type ReifyAUs r
  = forall rr. HFoldlWithIndex ReifyAUFoldingWithIndex {} r rr => rr

-- | Reify many audio units.
reifyAUs ::
  forall r rr.
  HFoldlWithIndex
    ReifyAUFoldingWithIndex
    {}
    r
    rr =>
  r -> rr
reifyAUs = hfoldlWithIndex ReifyAUFoldingWithIndex {}
