module WAGS.Graph.Constructors where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy)

-- | Term-level constructor for an allpass filter.
-- | - `frequency` - the frequency where the phase transition occurs.
-- | - `q` - the width of the filter.
-- | - `audioUnit` - the audio unit to filter.
data Allpass frequency q audioUnit
  = Allpass frequency q audioUnit

-- | Term-level constructor for a bandpass filter.
-- | - `frequency` - the frequency of the isolated band.
-- | - `q` - the width of the filter.
-- | - `audioUnit` - the audio unit to filter.
data Bandpass frequency q audioUnit
  = Bandpass frequency q audioUnit

-- | Term-level constructor for a constant value, aka DC offset.
-- | - `OnOff` - whether the generator is on or off.
-- | - `offset` - the amount of DC offset.
data Constant offset
  = Constant OnOff offset

-- | Term-level constructor for a convolver, aka reverb.
-- | - `buffer` - the buffer of the impulse response of the space.
-- | - `audioUnit` - the audio unit to convolve with the input response.
data Convolver (buffer :: Symbol) audioUnit
  = Convolver (Proxy buffer) audioUnit

-- | Term-level constructor for a delay unit.
-- | - `delay` - the delay to apply.
-- | - `audioUnit` - the audio unit to delay.
data Delay delay audioUnit
  = Delay delay audioUnit

-- | Term-level constructor for a duplicated audio unit.
-- | - `audioUnit` - the audio unit to duplicate.
-- | - `continuation` - the continuation of the audio graph. This is a function that takes the duplicated audio unit as its single argument.
data Dup audioUnit continuation
  = Dup audioUnit continuation

-- | Term-level constructor for a compressor.
-- | - `threshold` - The threshold under which compression kicks in.
-- | - `knee` - The kink of the compression.
-- | - `ratio` - The amount of compression to apply.
-- | - `attack` - How far we look ahead. Longer attacks will lead to more crisp compression at the expense of an audible delay.
-- | - `release` - How long the release time of compression should be.
data DynamicsCompressor threshold knee ratio attack release audioUnit
  = DynamicsCompressor threshold knee ratio attack release audioUnit

-- | Term-level constructor for a gain unit.
-- | - `volume` - the volume of the gain from 0 to 1.
-- | - `audioUnit` - what we are applying the gain to.
data Gain volume audioUnit
  = Gain volume audioUnit

-- | Term-level constructor for a highpass filter.
-- | - `frequency` - the frequency below which we start to filter.
-- | - `q` - the width of the filter.
-- | - `audioUnit` - the audio unit to filter.
data Highpass frequency q audioUnit
  = Highpass frequency q audioUnit

-- | Term-level constructor for a highshelf filter.
-- | - `frequency` - the frequency above which we start to filter.
-- | - `gain` - the boost or the amount of attenuation to apply.
-- | - `audioUnit` - the audio unit to filter.
data Highshelf frequency gain audioUnit
  = Highshelf frequency gain audioUnit

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
-- | - `audioUnit` - the audio unit to filter.
data Lowpass frequency q audioUnit
  = Lowpass frequency q audioUnit

-- | Term-level constructor for a lowshelf filter.
-- | - `frequency` - the frequency below which we start to filter.
-- | - `q` - the width of the filter.
-- | - `audioUnit` - the audio unit to filter.
data Lowshelf frequency gain audioUnit
  = Lowshelf frequency gain audioUnit

-- | Term-level constructor for a microphone
data Microphone
  = Microphone

-- | Term-level constructor for a notch (aka band-reject) filter.
-- | - `frequency` - the frequency we are rejecting.
-- | - `q` - the width of the filter.
-- | - `audioUnit` - the audio unit to filter.
data Notch frequency q audioUnit
  = Notch frequency q audioUnit

-- | Term-level constructor for a peaking filter. A peaking filter is a combination of bandpass and notch where the gain parameter modulates whether we are reinforcing or attenuating a frequency.
-- | - `frequency` - the frequency we are emphasizing _or_ rejecting.
-- | - `q` - the width of the filter.
-- | - `gain` - if positive, we are emphasizing the frequency. If negative, we are rejecting it.
-- | - `audioUnit` - the audio unit to filter.
data Peaking frequency q gain audioUnit
  = Peaking frequency q gain audioUnit

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
-- | - `audioUnit` - the audio unit to record.
data Recorder (recorder :: Symbol) audioUnit
  = Recorder (Proxy recorder) audioUnit

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
-- | - `audioUnit` - the audio unit to play from the loudspeaker.
data Speaker audioUnit
  = Speaker audioUnit

-- | Term-level constructor for a square-wave oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data SquareOsc frequency
  = SquareOsc OnOff frequency

-- | Term-level constructor for a stereo panner.
-- | - `pan` - the amount of pan to apply, where -1.0 is fully to the left and 1.0 is fully to the right.
-- | - `audioUnit` - the audio unit to pan.
data StereoPanner pan audioUnit
  = StereoPanner pan audioUnit

-- | Term-level constructor for a triangle oscillator.
-- | - `OnOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
data TriangleOsc frequency
  = TriangleOsc OnOff frequency

-- | Term-level constructor for a WaveShaper, aka distortion.
-- | - `floatArray` - the shape of the distortion.
-- | - `oversample` - how much to oversample - none, 2x or 4x. Once set, this cannot change without destroying and remaking the audio unit.
-- | - `audioUnit` - the audio unit to which we apply distortion.
data WaveShaper (floatArray :: Symbol) oversample audioUnit
  = WaveShaper (Proxy floatArray) oversample audioUnit

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
