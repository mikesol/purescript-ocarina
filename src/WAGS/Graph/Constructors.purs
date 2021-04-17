module WAGS.Graph.Constructors where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy)

-- | Term-level constructor for an allpass filter.
data Allpass frequency q audioUnit
  = Allpass frequency q audioUnit

-- | Term-level constructor for a bandpass filter.
data Bandpass frequency q audioUnit
  = Bandpass frequency q audioUnit

-- | Term-level constructor for a constant value.
data Constant offset
  = Constant OnOff offset

-- | Term-level constructor for a convolver, aka reverb.
data Convolver (buffer :: Symbol) audioUnit
  = Convolver (Proxy buffer) audioUnit

-- | Term-level constructor for a delay unit.
data Delay delay audioUnit
  = Delay delay audioUnit

-- | Term-level constructor for a duplicated audio unit.
data Dup audioUnit continuation
  = Dup audioUnit continuation

data DynamicsCompressor threshold knee ratio attack release audioUnit
  = DynamicsCompressor threshold knee ratio attack release audioUnit

data Gain volume audioUnit
  = Gain volume audioUnit

data Highpass frequency q audioUnit
  = Highpass frequency q audioUnit

data Highshelf frequency gain audioUnit
  = Highshelf frequency gain audioUnit

data LoopBuf (buffer :: Symbol) playbackRate
  = LoopBuf (Proxy buffer) OnOff playbackRate Number Number

data Lowpass frequency q audioUnit
  = Lowpass frequency q audioUnit

data Lowshelf frequency gain audioUnit
  = Lowshelf frequency gain audioUnit

data Microphone
  = Microphone

data Notch frequency q audioUnit
  = Notch frequency q audioUnit

data Peaking frequency q gain audioUnit
  = Peaking frequency q gain audioUnit

data PeriodicOsc (periodicOsc :: Symbol) frequency
  = PeriodicOsc (Proxy periodicOsc) OnOff frequency

data PlayBuf (buffer :: Symbol) playbackRate
  = PlayBuf (Proxy buffer) Number OnOff playbackRate

data Recorder (recorder :: Symbol) audioUnit
  = Recorder (Proxy recorder) audioUnit

data SawtoothOsc frequency
  = SawtoothOsc OnOff frequency

data SinOsc frequency
  = SinOsc OnOff frequency

data Speaker audioUnit
  = Speaker audioUnit

data SquareOsc frequency
  = SquareOsc OnOff frequency

data StereoPanner pan audioUnit
  = StereoPanner pan audioUnit

data TriangleOsc frequency
  = TriangleOsc OnOff frequency

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
