module WAGS.Graph.AudioUnit where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy)

-- | Term-level constructor for an allpass filter.
-- | - `frequency` - the frequency where the phase transition occurs.
-- | - `q` - the width of the filter.
data Allpass frequency q
  = Allpass frequency q


-- | Term-level constructor for a bandpass filter.
-- | - `frequency` - the frequency of the isolated band.
-- | - `q` - the width of the filter.
data Bandpass frequency q
  = Bandpass frequency q


-- | Term-level constructor for a constant value, aka DC offset.
-- | - `OnOff` - whether the generator is on or off.
-- | - `offset` - the amount of DC offset.
data Constant offset
  = Constant OnOff offset


-- | Term-level constructor for a convolver, aka reverb.
-- | - `buffer` - the buffer of the impulse response of the space.
data Convolver (buffer :: Symbol)
  = Convolver (Proxy buffer)

-- | Term-level constructor for a delay unit.
-- | - `delay` - the delay to apply.
data Delay delay
  = Delay delay


-- | Term-level constructor for a compressor.
-- | - `threshold` - The threshold under which compression kicks in.
-- | - `knee` - The kink of the compression.
-- | - `ratio` - The amount of compression to apply.
-- | - `attack` - How far we look ahead. Longer attacks will lead to more crisp compression at the expense of an audible delay.
-- | - `release` - How long the release time of compression should be.
data DynamicsCompressor threshold knee ratio attack release
  = DynamicsCompressor threshold knee ratio attack release


-- | Term-level constructor for a gain unit.
-- | - `volume` - the volume of the gain from 0 to 1.
data Gain volume
  = Gain volume


-- | Term-level constructor for a highpass filter.
-- | - `frequency` - the frequency below which we start to filter.
-- | - `q` - the width of the filter.
data Highpass frequency q
  = Highpass frequency q


-- | Term-level constructor for a highshelf filter.
-- | - `frequency` - the frequency above which we start to filter.
-- | - `gain` - the boost or the amount of attenuation to apply.
data Highshelf frequency gain
  = Highshelf frequency gain


-- | Term-level constructor for a looping buffer.
-- | - `buffer` - a string representing the buffer to use. Note that this string, when reset, will only reset the buffer when it is stopped.
-- | - `OnOff` - whether or not the generator is on or off.
-- | - `playbackRate` - the playback rate.
-- | - `Number` - where in the file the loop should start.
-- | - `Number` - where in the file the loop should end. A value of 0.0 or less means play to the end of the buffer.
data LoopBuf playbackRate
  = LoopBuf String OnOff playbackRate Number Number

-- | Term-level constructor for a lowpass filter.
-- | - `frequency` - the frequency above which we start to filter.
-- | - `q` - the width of the filter.
data Lowpass frequency q
  = Lowpass frequency q

-- | Term-level constructor for a lowshelf filter.
-- | - `frequency` - the frequency below which we start to filter.
-- | - `q` - the width of the filter.
data Lowshelf frequency gain
  = Lowshelf frequency gain

-- | Term-level constructor for a microphone
data Microphone
  = Microphone

-- | Term-level constructor for a notch (aka band-reject) filter.
-- | - `frequency` - the frequency we are rejecting.
-- | - `q` - the width of the filter.
data Notch frequency q
  = Notch frequency q

-- | Term-level constructor for a peaking filter. A peaking filter is a combination of bandpass and notch where the gain parameter modulates whether we are reinforcing or attenuating a frequency.
-- | - `frequency` - the frequency we are emphasizing _or_ rejecting.
-- | - `q` - the width of the filter.
-- | - `gain` - if positive, we are emphasizing the frequency. If negative, we are rejecting it.
data Peaking frequency q gain
  = Peaking frequency q gain

-- | Term-level constructor for a periodic oscillator.
-- | - `periodicOsc` - the name of the wave table we'll be using. Note that, for a chance to take effect, the periodic oscillator must be stopped.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data PeriodicOsc frequency
  = PeriodicOsc String OnOff frequency

-- | Term-level constructor for a playback buffer.
-- | - `buffer` - a string representing the buffer to use. Note that this string, when reset, will only reset the buffer when it is stopped.
-- | - `Number` - where in the file the playback should start.
-- | - `OnOff` - whether or not the generator is on or off.
-- | - `playbackRate` - the playback rate.
data PlayBuf playbackRate
  = PlayBuf String Number OnOff playbackRate

-- | Term-level constructor for a recorder.
-- | - `recorder` - the recorder to which we write data.
data Recorder (recorder :: Symbol)
  = Recorder (Proxy recorder)

-- | Term-level constructor for a sawtooth oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data SawtoothOsc frequency
  = SawtoothOsc OnOff frequency

-- | Term-level constructor for a sine-wave oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data SinOsc frequency
  = SinOsc OnOff frequency

-- | Term-level constructor for a loudspeaker.
data Speaker
  = Speaker

-- | Term-level constructor for a square-wave oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data SquareOsc frequency
  = SquareOsc OnOff frequency

-- | Term-level constructor for a stereo panner.
-- | - `pan` - the amount of pan to apply, where -1.0 is fully to the left and 1.0 is fully to the right.
data StereoPanner pan
  = StereoPanner pan

-- | Term-level constructor for a triangle oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data TriangleOsc frequency
  = TriangleOsc OnOff frequency

-- | Term-level constructor for a WaveShaper, aka distortion.
-- | - `floatArray` - the shape of the distortion.
-- | - `oversample` - how much to oversample - none, 2x or 4x. Once set, this cannot change without destroying and remaking the audio unit.
data WaveShaper (floatArray :: Symbol) oversample
  = WaveShaper (Proxy floatArray) oversample

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

-- | Type-level oversample 2x for a wave shaper. This is at the type-level and not the term-level via an ADT because we need make sure to construct an entirely new wave shaper if the value changes.
data OversampleTwoX
  = OversampleTwoX

-- | Type-level oversample 4x for a wave shaper. This is at the type-level and not the term-level via an ADT because we need make sure to construct an entirely new wave shaper if the value changes.
data OversampleFourX
  = OversampleFourX

-- | Type-level constructor for an allpass filter.
data TAllpass

-- | Type-level constructor for a bandpass filter.
data TBandpass 

-- | Type-level constructor for a constant value.
data TConstant 

-- | Type-level constructor for a convolver, aka reverb.
data TConvolver (sym :: Symbol)

-- | Type-level constructor for a delay unit.
data TDelay 

-- | Type-level constructor for a compressor.
data TDynamicsCompressor 

-- | Type-level constructor for a gain unit.
data TGain 

-- | Type-level constructor for a highpass filter.
data THighpass 

-- | Type-level constructor for a highshelf filter.
data THighshelf 

-- | Type-level constructor for a looping buffer.
data TLoopBuf 

-- | Type-level constructor for a lowpass filter.
data TLowpass 

-- | Type-level constructor for a lowshelf filter.
data TLowshelf 

-- | Type-level constructor for a microphone.
data TMicrophone 

-- | Type-level constructor for a notch filter.
data TNotch 

-- | Type-level constructor for a peaking filter.
data TPeaking 

-- | Type-level constructor for a periodic oscillator.
data TPeriodicOsc 

-- | Type-level constructor for playback from a buffer.
data TPlayBuf 

-- | Type-level constructor for a recorder.
data TRecorder (sym :: Symbol)

-- | Type-level constructor for a sawtooth oscillator.
data TSawtoothOsc 

-- | Type-level constructor for a sine-wave oscillator.
data TSinOsc 

-- | Type-level constructor for a loudspeaker.
data TSpeaker 

-- | Type-level constructor for a square-wave oscillator.
data TSquareOsc 

-- | Type-level constructor for a stereo panner.
data TStereoPanner 

-- | Type-level constructor for a triangle oscillator.
data TTriangleOsc 

-- | Type-level constructor for a wave shaper.
data TWaveShaper (sym :: Symbol) (oversample :: Type)