module WAGS.Tumult.Graph.AudioUnit where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Data.Variant.Maybe (Maybe)
import WAGS.Core as Core
import Prim.Symbol as Sym
import Type.Proxy (Proxy(..))
import WAGS.Interpret (AudioWorkletNodeResponse)
import WAGS.Parameter (AudioOnOff, AudioParameter)
import WAGS.WebAPI (BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserMicrophone, MediaRecorderCb)

class TypeToSym (a :: Type) (b :: Symbol) | a -> b

instance typeToSymTup :: TypeToSym a c => TypeToSym (a /\ b) c

-- | Term-level constructor for an allpass filter.
-- | - `frequency` - the frequency where the phase transition occurs.
-- | - `q` - the width of the filter.

type Allpass' = (frequency :: AudioParameter, q :: AudioParameter)
newtype Allpass = Allpass { | Allpass' }

derive instance newtypeAllpass :: Newtype Allpass _
instance typeToSymAllpass :: TypeToSym Allpass "Allpass"

-- | Term-level constructor for a analyser.
-- | - `analyser` - the analyser to which we write data.
data Analyser callback = Analyser callback

instance typeToSymAnalyser :: TypeToSym (Analyser callback) "Analyser"

type AudioWorkletNodeOptions'
  (numberOfInputs :: Type)
  (numberOfOutputs :: Type)
  (outputChannelCount :: Type)
  (parameterData :: Row Type)
  (processorOptions :: Row Type) =
  ( numberOfInputs :: numberOfInputs
  , numberOfOutputs :: numberOfOutputs
  , outputChannelCount :: outputChannelCount
  , parameterData :: { | parameterData }
  , processorOptions :: { | processorOptions }
  )

newtype AudioWorkletNodeOptions
  (numberOfInputs :: Type)
  (numberOfOutputs :: Type)
  (outputChannelCount :: Type)
  (parameterData :: Row Type)
  (processorOptions :: Row Type) = AudioWorkletNodeOptions
  {
  | AudioWorkletNodeOptions' numberOfInputs numberOfOutputs outputChannelCount
      parameterData
      processorOptions
  }

-- | Term-level constructor for an audio worklet node.
-- | - `node` - the name of the node.
-- | - `options` - initialization options
data AudioWorkletNode
  (node :: Symbol)
  (numberOfInputs :: Type)
  (numberOfOutputs :: Type)
  (outputChannelCount :: Type)
  (parameterData :: Row Type)
  (processorOptions :: Row Type) = AudioWorkletNode
  ( AudioWorkletNodeResponse node numberOfInputs numberOfOutputs
      outputChannelCount
      parameterData
      processorOptions
  )
  ( AudioWorkletNodeOptions numberOfInputs numberOfOutputs outputChannelCount
      parameterData
      processorOptions
  )

instance typeToSymAudioWorkletNode ::
  Sym.Append "AudioWorkletNode" sym o =>
  TypeToSym ( AudioWorkletNode sym numberOfInputs numberOfOutputs
        outputChannelCount
        parameterData
        processorOptions
    )
    o

-- | Term-level constructor for a bandpass filter.
-- | - `frequency` - the frequency of the isolated band.
-- | - `q` - the width of the filter.
type Bandpass' = (frequency :: AudioParameter, q :: AudioParameter)
newtype Bandpass = Bandpass { | Bandpass' }

derive instance newtypeBandpass :: Newtype Bandpass _
instance typeToSymBandpass :: TypeToSym Bandpass "Bandpass"

-- | Term-level constructor for a constant value, aka DC offset.
-- | - `onOff` - whether the generator is on or off.
-- | - `offset` - the amount of DC offset.
type Constant' = (onOff :: AudioOnOff, offset :: AudioParameter)
newtype Constant = Constant { | Constant' }

derive instance newtypeConstant :: Newtype Constant _
instance typeToSymConstant :: TypeToSym Constant "Constant"

-- | Term-level constructor for a convolver, aka reverb.
-- | - `buffer` - the buffer of the impulse response of the space.
type Convolver' = (buffer :: BrowserAudioBuffer)
newtype Convolver = Convolver { | Convolver' }

derive instance newtypeConvolver :: Newtype (Convolver) _
instance typeToSymConvolver :: TypeToSym (Convolver) "Convolver"

-- | Term-level constructor for a delay unit.
-- | - `delay` - the delay to apply.
type Delay' = (delayTime :: AudioParameter)
newtype Delay = Delay { | Delay' }

derive instance newtypeDelay :: Newtype Delay _
instance typeToSymDelay :: TypeToSym Delay "Delay"

-- | Term-level constructor for a compressor.
-- | - `threshold` - The threshold under which compression kicks in.
-- | - `knee` - The kink of the compression.
-- | - `ratio` - The amount of compression to apply.
-- | - `attack` - How far we look ahead. Longer attacks will lead to more crisp compression at the expense of an audible delay.
-- | - `release` - How long the release time of compression should be.
type DynamicsCompressor' =
  ( threshold :: AudioParameter
  , knee :: AudioParameter
  , ratio :: AudioParameter
  , attack :: AudioParameter
  , release :: AudioParameter
  )

newtype DynamicsCompressor = DynamicsCompressor { | DynamicsCompressor' }

derive instance newtypeDynamicsCompressor :: Newtype DynamicsCompressor _
instance typeToSymDynamicsCompressor ::
  TypeToSym DynamicsCompressor "DynamicsCompressor"

-- | Term-level constructor for a gain unit.
-- | - `gain` - the volume of the gain from 0 to 1.
type Gain' = (gain :: AudioParameter)
newtype Gain = Gain { | Gain' }

derive instance newtypeGain :: Newtype Gain _
instance typeToSymGain :: TypeToSym Gain "Gain"

-- | Term-level constructor for a highpass filter.
-- | - `frequency` - the frequency below which we start to filter.
-- | - `q` - the width of the filter.
type Highpass' = (frequency :: AudioParameter, q :: AudioParameter)
newtype Highpass = Highpass { | Highpass' }

derive instance newtypeHighpass :: Newtype Highpass _
instance typeToSymHighpass :: TypeToSym Highpass "Highpass"

-- | Term-level constructor for a highshelf filter.
-- | - `frequency` - the frequency above which we start to filter.
-- | - `gain` - the boost or the amount of attenuation to apply.
type Highshelf' = (frequency :: AudioParameter, gain :: AudioParameter)
newtype Highshelf = Highshelf { | Highshelf' }

derive instance newtypeHighshelf :: Newtype Highshelf _
instance typeToSymHighshelf :: TypeToSym Highshelf "Highshelf"

-- | Term-level constructor for arbitrary input (ie from another audio graph)
-- | - `input` - the input to use.
data Input (input :: Symbol) = Input
instance typeToSymInput :: TypeToSym (Input input) "Input"
instance typeToSymCoreInput :: TypeToSym Core.Input "Input"

-- | Term-level constructor for a looping buffer.
-- | - `buffer` - the buffer to use. Note that this symbol, when reset, will only reset the buffer when it is stopped.
-- | - `onOff` - whether or not the generator is on or off.
-- | - `playbackRate` - the playback rate.
-- | - `loopStart` - where in the file the loop should start.
-- | - `loopEnd` - where in the file the loop should end. A value of 0.0 or less means play to the end of the buffer.

type LoopBuf' =
  ( buffer :: BrowserAudioBuffer
  , onOff :: AudioOnOff
  , playbackRate :: AudioParameter
  , loopStart :: Number
  , loopEnd :: Number
  , duration :: Maybe Number
  )

newtype LoopBuf = LoopBuf { | LoopBuf' }

derive instance newtypeLoopBuf :: Newtype LoopBuf _
instance typeToSymLoopBuf :: TypeToSym LoopBuf "LoopBuf"

-- | Term-level constructor for a lowpass filter.
-- | - `frequency` - the frequency above which we start to filter.
-- | - `q` - the width of the filter.
type Lowpass' = (frequency :: AudioParameter, q :: AudioParameter)
newtype Lowpass = Lowpass { | Lowpass' }

derive instance newtypeLowpass :: Newtype Lowpass _
instance typeToSymLowpass :: TypeToSym Lowpass "Lowpass"

-- | Term-level constructor for a lowshelf filter.
-- | - `frequency` - the frequency below which we start to filter.
-- | - `gain` - the width of the filter.
type Lowshelf' = (frequency :: AudioParameter, gain :: AudioParameter)
newtype Lowshelf = Lowshelf { | Lowshelf' }

derive instance newtypeLowshelf :: Newtype Lowshelf _
instance typeToSymLowshelf :: TypeToSym Lowshelf "Lowshelf"
-- | Term-level constructor for a media element.
-- | - `element` - the media element to use.
type MediaElement' = (element :: BrowserMediaElement)
newtype MediaElement = MediaElement { | MediaElement' }

derive instance newtypeMediaElement :: Newtype MediaElement _

instance typeToSymMediaElement :: TypeToSym (MediaElement) "MediaElement"

-- | Term-level constructor for a microphone
newtype Microphone = Microphone { microphone :: BrowserMicrophone }

instance typeToSymMicrophone :: TypeToSym (Microphone) "Microphone"

-- | Term-level constructor for a notch (aka band-reject) filter.
-- | - `frequency` - the frequency we are rejecting.
-- | - `q` - the width of the filter.
type Notch' = (frequency :: AudioParameter, q :: AudioParameter)
newtype Notch = Notch { | Notch' }

derive instance newtypeNotch :: Newtype Notch _
instance typeToSymNotch :: TypeToSym Notch "Notch"

-- | Term-level constructor for a peaking filter. A peaking filter is a combination of bandpass and notch where the gain parameter modulates whether we are reinforcing or attenuating a frequency.
-- | - `frequency` - the frequency we are emphasizing _or_ rejecting.
-- | - `q` - the width of the filter.
-- | - `gain` - if positive, we are emphasizing the frequency. If negative, we are rejecting it.
type Peaking' =
  ( frequency :: AudioParameter
  , q :: AudioParameter
  , gain :: AudioParameter
  )

newtype Peaking = Peaking { | Peaking' }

derive instance newtypePeaking :: Newtype Peaking _
instance typeToSymPeaking :: TypeToSym Peaking "Peaking"
-- | Term-level constructor for a periodic oscillator.
-- | - `periodicOsc` - the name of the wave table we'll be using. Note that, for a chance to take effect, the periodic oscillator must be stopped.
-- | - `onOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
type PeriodicOsc' periodicOsc =
  (wave :: periodicOsc, onOff :: AudioOnOff, frequency :: AudioParameter)

newtype PeriodicOsc periodicOsc = PeriodicOsc { | PeriodicOsc' periodicOsc }

derive instance newtypePeriodicOsc :: Newtype (PeriodicOsc periodicOsc) _
instance typeToSymPeriodicOsc ::
  TypeToSym (PeriodicOsc periodicOsc) "PeriodicOsc"

-- | Term-level constructor for a playback buffer.
-- | - `buffer` - the buffer to use. Note that this symbol, when reset, will only reset the buffer when it is stopped.
-- | - `offset` - where in the file the playback should start.
-- | - `onOff` - whether or not the generator is on or off.
-- | - `playbackRate` - the playback rate.
type PlayBuf' =
  ( buffer :: BrowserAudioBuffer
  , onOff :: AudioOnOff
  , playbackRate :: AudioParameter
  , bufferOffset :: Number
  , duration :: Maybe Number
  )

newtype PlayBuf = PlayBuf { | PlayBuf' }

derive instance newtypePlayBuf :: Newtype PlayBuf _
instance typeToSymPlayBuf :: TypeToSym PlayBuf "PlayBuf"

-- | Term-level constructor for a recorder.
-- | - `recorder` - the recorder to which we write data.
type Recorder' = (cb :: MediaRecorderCb)
newtype Recorder = Recorder { | Recorder' }
derive instance newtypeRecorder :: Newtype Recorder _
instance typeToSymRecorder :: TypeToSym (Recorder) "Recorder"

-- | Term-level constructor for a sawtooth oscillator.
-- | - `onOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
type SawtoothOsc' = (onOff :: AudioOnOff, frequency :: AudioParameter)
newtype SawtoothOsc = SawtoothOsc { | SawtoothOsc' }

derive instance newtypeSawtoothOsc :: Newtype SawtoothOsc _
instance typeToSymSawtoothOsc :: TypeToSym SawtoothOsc "SawtoothOsc"

-- | Term-level constructor for a sine-wave oscillator.
-- | - `onOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
type SinOsc' = (onOff :: AudioOnOff, frequency :: AudioParameter)
newtype SinOsc = SinOsc { | SinOsc' }

derive instance newtypeSinOsc :: Newtype SinOsc _
instance typeToSymSinOsc :: TypeToSym SinOsc "SinOsc"

-- | Term-level constructor for a loudspeaker.
data Speaker = Speaker

instance typeToSymSpeaker :: TypeToSym Speaker "Speaker"

-- | Term-level constructor for a square-wave oscillator.
-- | - `onOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
type SquareOsc' = (onOff :: AudioOnOff, frequency :: AudioParameter)
newtype SquareOsc = SquareOsc { | SquareOsc' }

derive instance newtypeSquareOsc :: Newtype SquareOsc _
instance typeToSymSquareOsc :: TypeToSym SquareOsc "SquareOsc"

-- | Term-level constructor for a stereo panner.
-- | - `pan` - the amount of pan to apply, where -1.0 is fully to the left and 1.0 is fully to the right.
type StereoPanner' = (pan :: AudioParameter)
newtype StereoPanner = StereoPanner { | StereoPanner' }

derive instance newtypeStereoPanner :: Newtype StereoPanner _
instance typeToSymStereoPanner :: TypeToSym StereoPanner "StereoPanner"

-- | Term-level constructor for a triangle oscillator.
-- | - `onOff` - whether the generator is on or off.
-- | - `frequency` - the frequency of the oscillator.
type TriangleOsc' = (onOff :: AudioOnOff, frequency :: AudioParameter)
newtype TriangleOsc = TriangleOsc { | TriangleOsc' }
derive instance newtypeTriangleOsc :: Newtype TriangleOsc _
instance typeToSymTriangleOsc :: TypeToSym TriangleOsc "TriangleOsc"

-- | Term-level constructor for a WaveShaper, aka distortion.
-- | - `floatArray` - the shape of the distortion.
-- | - `oversample` - how much to oversample - none, 2x or 4x. Once set, this cannot change without destroying and remaking the audio unit.
type WaveShaper' oversample =
  (floatArray :: BrowserFloatArray, oversample :: oversample)

newtype WaveShaper oversample = WaveShaper
  { | WaveShaper' oversample }
derive instance newtypeWaveShaper :: Newtype (WaveShaper oversample) _
instance typeToSymWaveShaper ::
  TypeToSym (WaveShaper oversample) "WaveShaper"

-- | Type-level oversample none for a wave shaper. This is at the type-level and not the term-level via an ADT because we need make sure to construct an entirely new wave shaper if the value changes.
data OversampleNone = OversampleNone

instance semigroupOversampleNone :: Semigroup OversampleNone where
  append _ _ = OversampleNone

instance monoidOversampleNone :: Monoid OversampleNone where
  mempty = OversampleNone

-- | Type-level oversample 2x for a wave shaper. This is at the type-level and not the term-level via an ADT because we need make sure to construct an entirely new wave shaper if the value changes.
data OversampleTwoX = OversampleTwoX

instance semigroupOversampleTwoX :: Semigroup OversampleTwoX where
  append _ _ = OversampleTwoX

instance monoidOversampleTwoX :: Monoid OversampleTwoX where
  mempty = OversampleTwoX

-- | Type-level oversample 4x for a wave shaper. This is at the type-level and not the term-level via an ADT because we need make sure to construct an entirely new wave shaper if the value changes.
data OversampleFourX = OversampleFourX

instance semigroupOversampleFourX :: Semigroup OversampleFourX where
  append _ _ = OversampleFourX

instance monoidOversampleFourX :: Monoid OversampleFourX where
  mempty = OversampleFourX

class ReifyAU a b | a -> b where
  reifyAU :: a -> b

-- | Type-level constructor for an allpass filter.
data TAllpass = TAllpass

instance typeToSymTAllpass :: TypeToSym TAllpass "TAllpass"

instance semigroupTAllpass :: Semigroup TAllpass where
  append _ _ = TAllpass

instance monoidTAllpass :: Monoid TAllpass where
  mempty = TAllpass

instance reifyTAllpass :: ReifyAU Allpass TAllpass where
  reifyAU = const mempty

-- | Type-level constructor for an analyser.
data TAnalyser = TAnalyser

instance typeToSymTAnalyser :: TypeToSym TAnalyser "TAllpass"

instance semigroupTAnalyser :: Semigroup TAnalyser where
  append _ _ = TAnalyser

instance monoidTAnalyser :: Monoid TAnalyser where
  mempty = TAnalyser

instance reifyTAnalyser :: ReifyAU (Analyser callback) TAnalyser where
  reifyAU = const mempty

-- | Type-level constructor for an audio worklet node.
data TAudioWorkletNode
  (sym :: Symbol)
  (numberOfInputs :: Type)
  (numberOfOutputs :: Type)
  (outputChannelCount :: Type)
  (parameterData :: Row Type)
  (processorOptions :: Row Type) = TAudioWorkletNode (Proxy sym)
  (Proxy numberOfInputs)
  (Proxy numberOfOutputs)
  (Proxy outputChannelCount)
  (Proxy parameterData)
  (Proxy processorOptions)

instance typeToSymTAudioWorkletNode ::
  Sym.Append "TAudioWorkletNode_" sym o =>
  TypeToSym ( TAudioWorkletNode sym numberOfInputs numberOfOutputs
        outputChannelCount
        parameterData
        processorOptions
    )
    o

instance semigroupTAudioWorkletNode ::
  Semigroup ( TAudioWorkletNode sym numberOfInputs numberOfOutputs
        outputChannelCount
        parameterData
        processorOptions
    ) where
  append _ _ = TAudioWorkletNode (Proxy :: _ sym) (Proxy :: _ numberOfInputs)
    (Proxy :: _ numberOfOutputs)
    (Proxy :: _ outputChannelCount)
    (Proxy :: _ parameterData)
    (Proxy :: _ processorOptions)

instance monoidTAudioWorkletNode ::
  Monoid ( TAudioWorkletNode sym numberOfInputs numberOfOutputs
        outputChannelCount
        parameterData
        processorOptions
    ) where
  mempty = TAudioWorkletNode (Proxy :: _ sym) (Proxy :: _ numberOfInputs)
    (Proxy :: _ numberOfOutputs)
    (Proxy :: _ outputChannelCount)
    (Proxy :: _ parameterData)
    (Proxy :: _ processorOptions)

instance reifyTAudioWorkletNode ::
  ReifyAU ( AudioWorkletNode sym numberOfInputs numberOfOutputs
        outputChannelCount
        parameterData
        processorOptions
    )
    ( TAudioWorkletNode sym numberOfInputs numberOfOutputs outputChannelCount
        parameterData
        processorOptions
    ) where
  reifyAU = const mempty

-- | Type-level constructor for a bandpass filter.
data TBandpass = TBandpass

instance typeToSymTBandpass :: TypeToSym TBandpass "TBandpass"

instance semigroupTBandpass :: Semigroup TBandpass where
  append _ _ = TBandpass

instance monoidTBandpass :: Monoid TBandpass where
  mempty = TBandpass

instance reifyTBandpass :: ReifyAU Bandpass TBandpass where
  reifyAU = const mempty

-- | Type-level constructor for a constant value.
data TConstant = TConstant

instance typeToSymTConstant :: TypeToSym TConstant "TConstant"

instance semigroupTConstant :: Semigroup TConstant where
  append _ _ = TConstant

instance monoidTConstant :: Monoid TConstant where
  mempty = TConstant

instance reifyTConstant :: ReifyAU Constant TConstant where
  reifyAU = const mempty

-- | Type-level constructor for a convolver, aka reverb.
data TConvolver = TConvolver

instance typeToSymTConvolver :: TypeToSym TConvolver "TConvolver"

instance semigroupTConvolver :: Semigroup TConvolver where
  append _ _ = TConvolver

instance monoidTConvolver :: Monoid TConvolver where
  mempty = TConvolver

instance reifyTConvolver :: ReifyAU (Convolver) TConvolver where
  reifyAU = const mempty

-- | Type-level constructor for a delay unit.
data TDelay = TDelay

instance typeToSymTDelay :: TypeToSym TDelay "TDelay"

instance semigroupTDelay :: Semigroup TDelay where
  append _ _ = TDelay

instance monoidTDelay :: Monoid TDelay where
  mempty = TDelay

instance reifyTDelay :: ReifyAU Delay TDelay where
  reifyAU = const mempty

-- | Type-level constructor for a compressor.
data TDynamicsCompressor = TDynamicsCompressor

instance typeToSymTDynamicsCompressor ::
  TypeToSym TDynamicsCompressor "TDynamicsCompressor"

instance semigroupTDynamicsCompressor :: Semigroup TDynamicsCompressor where
  append _ _ = TDynamicsCompressor

instance monoidTDynamicsCompressor :: Monoid TDynamicsCompressor where
  mempty = TDynamicsCompressor

instance reifyTDynamicsCompressor ::
  ReifyAU DynamicsCompressor TDynamicsCompressor where
  reifyAU = const mempty

-- | Type-level constructor for a gain unit.
data TGain = TGain

instance typeToSymTGain :: TypeToSym TGain "TGain"

instance semigroupTGain :: Semigroup TGain where
  append _ _ = TGain

instance monoidTGain :: Monoid TGain where
  mempty = TGain

instance reifyTGain :: ReifyAU Gain TGain where
  reifyAU = const mempty

-- | Type-level constructor for a highpass filter.
data THighpass = THighpass

instance typeToSymTHighpass :: TypeToSym THighpass "THighpass"

instance semigroupTHighpass :: Semigroup THighpass where
  append _ _ = THighpass

instance monoidTHighpass :: Monoid THighpass where
  mempty = THighpass

instance reifyTHighpass :: ReifyAU (Highpass) THighpass where
  reifyAU = const mempty

-- | Type-level constructor for a highshelf filter.
data THighshelf = THighshelf

instance typeToSymTHighshelf :: TypeToSym THighshelf "THighshelf"

instance semigroupTHighshelf :: Semigroup THighshelf where
  append _ _ = THighshelf

instance monoidTHighshelf :: Monoid THighshelf where
  mempty = THighshelf

instance reifyTHighshelf :: ReifyAU (Highshelf) THighshelf where
  reifyAU = const mempty

-- | Type-level constructor for arbitrary input
data TInput = TInput

instance typeToSymTInput :: TypeToSym (TInput) "TInput"

instance semigroupTInput :: Semigroup (TInput) where
  append _ _ = TInput

instance monoidTInput :: Monoid (TInput) where
  mempty = TInput

-- | Type-level constructor for a looping buffer.
data TLoopBuf = TLoopBuf

instance typeToSymTLoopBuf :: TypeToSym TLoopBuf "TLoopBuf"

instance semigroupTLoopBuf :: Semigroup TLoopBuf where
  append _ _ = TLoopBuf

instance monoidTLoopBuf :: Monoid TLoopBuf where
  mempty = TLoopBuf

instance reifyTLoopBuf :: ReifyAU (LoopBuf) TLoopBuf where
  reifyAU = const mempty

-- | Type-level constructor for a lowpass filter.
data TLowpass = TLowpass

instance typeToSymTLowpass :: TypeToSym TLowpass "TLowpass"

instance semigroupTLowpass :: Semigroup TLowpass where
  append _ _ = TLowpass

instance monoidTLowpass :: Monoid TLowpass where
  mempty = TLowpass

instance reifyTLowpass :: ReifyAU (Lowpass) TLowpass where
  reifyAU = const mempty

-- | Type-level constructor for a lowshelf filter.
data TLowshelf = TLowshelf

instance typeToSymTLowshelf :: TypeToSym TLowshelf "TLowshelf"

instance semigroupTLowshelf :: Semigroup TLowshelf where
  append _ _ = TLowshelf

instance monoidTLowshelf :: Monoid TLowshelf where
  mempty = TLowshelf

instance reifyTLowshelf :: ReifyAU (Lowshelf) TLowshelf where
  reifyAU = const mempty

-- | Type-level constructor for playback from a media element.
data TMediaElement = TMediaElement

instance typeToSymTMediaElement :: TypeToSym TMediaElement "TMediaElement"

instance semigroupTMediaElement :: Semigroup TMediaElement where
  append _ _ = TMediaElement

instance monoidTMediaElement :: Monoid TMediaElement where
  mempty = TMediaElement

instance reifyTMediaElement :: ReifyAU (MediaElement) TMediaElement where
  reifyAU = const mempty

-- | Type-level constructor for a microphone.
data TMicrophone = TMicrophone

instance typeToSymTMicrophone :: TypeToSym TMicrophone "TMicrophone"

instance semigroupTMicrophone :: Semigroup TMicrophone where
  append _ _ = TMicrophone

instance monoidTMicrophone :: Monoid TMicrophone where
  mempty = TMicrophone

instance reifyTMicrophone :: ReifyAU (Microphone) TMicrophone where
  reifyAU = const mempty

-- | Type-level constructor for a notch filter.
data TNotch = TNotch

instance typeToSymTNotch :: TypeToSym TNotch "TNotch"

instance semigroupTNotch :: Semigroup TNotch where
  append _ _ = TNotch

instance monoidTNotch :: Monoid TNotch where
  mempty = TNotch

instance reifyTNotch :: ReifyAU (Notch) TNotch where
  reifyAU = const mempty

-- | Type-level constructor for a peaking filter.
data TPeaking = TPeaking

instance typeToSymTPeaking :: TypeToSym TPeaking "TPeaking"

instance semigroupTPeaking :: Semigroup TPeaking where
  append _ _ = TPeaking

instance monoidTPeaking :: Monoid TPeaking where
  mempty = TPeaking

instance reifyTPeaking :: ReifyAU (Peaking) TPeaking where
  reifyAU = const mempty

-- | Type-level constructor for a periodic oscillator.
data TPeriodicOsc = TPeriodicOsc

instance typeToSymTPeriodicOsc :: TypeToSym TPeriodicOsc "TPeriodicOsc"

instance semigroupTPeriodicOsc :: Semigroup TPeriodicOsc where
  append _ _ = TPeriodicOsc

instance monoidTPeriodicOsc :: Monoid TPeriodicOsc where
  mempty = TPeriodicOsc

instance reifyTPeriodicOsc :: ReifyAU (PeriodicOsc a) TPeriodicOsc where
  reifyAU = const mempty

-- | Type-level constructor for playback from a buffer.
data TPlayBuf = TPlayBuf

instance typeToSymTPlayBuf :: TypeToSym TPlayBuf "TPlayBuf"

instance semigroupTPlayBuf :: Semigroup TPlayBuf where
  append _ _ = TPlayBuf

instance monoidTPlayBuf :: Monoid TPlayBuf where
  mempty = TPlayBuf

instance reifyTPlayBuf :: ReifyAU (PlayBuf) TPlayBuf where
  reifyAU = const mempty

-- | Type-level constructor for a recorder.
data TRecorder = TRecorder

instance typeToSymTRecorder :: TypeToSym TRecorder "TRecorder"

instance semigroupTRecorder :: Semigroup TRecorder where
  append _ _ = TRecorder

instance monoidTRecorder :: Monoid TRecorder where
  mempty = TRecorder

instance reifyTRecorder :: ReifyAU (Recorder) TRecorder where
  reifyAU = const mempty

-- | Type-level constructor for a sawtooth oscillator.
data TSawtoothOsc = TSawtoothOsc

instance typeToSymTSawtoothOsc :: TypeToSym TSawtoothOsc "TSawtoothOsc"

instance semigroupTSawtoothOsc :: Semigroup TSawtoothOsc where
  append _ _ = TSawtoothOsc

instance monoidTSawtoothOsc :: Monoid TSawtoothOsc where
  mempty = TSawtoothOsc

instance reifyTSawtoothOsc :: ReifyAU (SawtoothOsc) TSawtoothOsc where
  reifyAU = const mempty

-- | Type-level constructor for a sine-wave oscillator.
data TSinOsc = TSinOsc

instance typeToSymTSinOsc :: TypeToSym TSinOsc "TSinOsc"

instance semigroupTSinOsc :: Semigroup TSinOsc where
  append _ _ = TSinOsc

instance monoidTSinOsc :: Monoid TSinOsc where
  mempty = TSinOsc

instance reifyTSinOsc :: ReifyAU (SinOsc) TSinOsc where
  reifyAU = const mempty

-- | Type-level constructor for a loudspeaker.
data TSpeaker = TSpeaker

instance typeToSymTSpeaker :: TypeToSym TSpeaker "TSpeaker"

instance semigroupTSpeaker :: Semigroup TSpeaker where
  append _ _ = TSpeaker

instance monoidTSpeaker :: Monoid TSpeaker where
  mempty = TSpeaker

instance reifyTSpeaker :: ReifyAU Speaker TSpeaker where
  reifyAU = const mempty

-- | Type-level constructor for a square-wave oscillator.
data TSquareOsc = TSquareOsc

instance typeToSymTSquareOsc :: TypeToSym TSquareOsc "TSquareOsc"

instance semigroupTSquareOsc :: Semigroup TSquareOsc where
  append _ _ = TSquareOsc

instance monoidTSquareOsc :: Monoid TSquareOsc where
  mempty = TSquareOsc

instance reifyTSquareOsc :: ReifyAU (SquareOsc) TSquareOsc where
  reifyAU = const mempty

-- | Type-level constructor for a stereo panner.
data TStereoPanner = TStereoPanner

instance typeToSymTStereoPanner :: TypeToSym TStereoPanner "TStereoPanner"

instance semigroupTStereoPanner :: Semigroup TStereoPanner where
  append _ _ = TStereoPanner

instance monoidTStereoPanner :: Monoid TStereoPanner where
  mempty = TStereoPanner

instance reifyTStereoPanner :: ReifyAU (StereoPanner) TStereoPanner where
  reifyAU = const mempty

-- | Type-level constructor for a triangle oscillator.
data TTriangleOsc = TTriangleOsc

instance typeToSymTTriangleOsc :: TypeToSym TTriangleOsc "TTriangleOsc"

instance semigroupTTriangleOsc :: Semigroup TTriangleOsc where
  append _ _ = TTriangleOsc

instance monoidTTriangleOsc :: Monoid TTriangleOsc where
  mempty = TTriangleOsc

instance reifyTTriangleOsc :: ReifyAU (TriangleOsc) TTriangleOsc where
  reifyAU = const mempty


-- | Type-level constructor for a wave shaper.
data TWaveShaper (oversample :: Type) = TWaveShaper oversample

instance typeToSymTWaveshaper2x ::
  TypeToSym (TWaveShaper OversampleTwoX) "TWaveShaper_OversampleTwoX"

instance typeToSymTWaveshaper4x ::
  TypeToSym (TWaveShaper OversampleFourX) "TWaveShaper_OversampleFourX"

instance typeToSymTWaveshaperNone ::
  TypeToSym (TWaveShaper OversampleNone) "TWaveShaper_OversampleNone"

instance semigroupTWaveShaper :: Monoid a => Semigroup (TWaveShaper a) where
  append _ _ = TWaveShaper mempty

instance monoidTWaveShaper :: Monoid a => Monoid (TWaveShaper a) where
  mempty = TWaveShaper mempty

instance reifyTWaveShaper ::
  Monoid b =>
  ReifyAU (WaveShaper b) (TWaveShaper b) where
  reifyAU = const mempty
