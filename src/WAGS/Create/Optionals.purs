-- | This module provides functions for the construction of audio units that more closely resemble the overloaded constructors of the Web Audio API.
module WAGS.Create.Optionals where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D1)
import Data.Vec as V
import Simple.JSON as JSON
import Type.Row.Homogeneous (class Homogeneous)
import WAGS.Change (ChangeSubgraph(..))
import WAGS.Control.Types (Frame0, SubScene)
import WAGS.Graph.AudioUnit (AudioWorkletNodeOptions)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Oversample (class IsOversample)
import WAGS.Graph.Paramable (class Paramable, onOffIze, paramize)
import WAGS.Graph.Parameter (AudioOnOff, OnOff, _on, AudioParameter)
import WAGS.Graph.Worklet (AudioWorkletNodeResponse)
import WAGS.Interpret (class AudioInterpret, AsSubgraph(..))
import WAGS.Tumult (Tumultuous)
import WAGS.Util (class ValidateOutputChannelCount)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

-----------
data Allpass
  = Allpass

instance convertAllpassFrequency :: Paramable a => ConvertOption Allpass "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertAllpassQ :: Paramable a => ConvertOption Allpass "q" a AudioParameter where
  convertOption _ _ = paramize

type AllpassOptional
  = (q :: AudioParameter)

type AllpassAll
  =
  ( freq :: AudioParameter
  | AllpassOptional
  )

defaultAllpass :: { | AllpassOptional }
defaultAllpass = { q: paramize 1.0 }

class AllpassCtor i allpass | i -> allpass where
  -- | Create an allpass filter, connecting it to another unit
  -- |
  -- | ```purescript
  -- | allpass { freq: 440.0 } { sinOsc: unit }
  -- | allpass { freq: 440.0, q: 1.0 } { sinOsc: unit }
  -- | allpass 440.0 { sinOsc: unit }
  -- | ```
  allpass :: i -> allpass

instance allpassCtor1 ::
  ( ConvertOptionsWithDefaults Allpass { | AllpassOptional } { | provided } { | AllpassAll }
  ) =>
  AllpassCtor { | provided } (b -> CTOR.Allpass AudioParameter AudioParameter /\ b) where
  allpass provided b = CTOR.Allpass all.freq all.q /\ b
    where
    all :: { | AllpassAll }
    all = convertOptionsWithDefaults Allpass defaultAllpass provided
else instance allpassCtor2 :: Paramable a => AllpassCtor a (b -> CTOR.Allpass AudioParameter AudioParameter /\ b) where
  allpass a b = CTOR.Allpass (paramize a) defaultAllpass.q /\ b

type CAllpass a
  = CTOR.Allpass AudioParameter AudioParameter /\ a

------
-- | Make an analyser.
-- |
-- | ```purescript
-- | analyser "track"
-- | ```
analyser
  :: forall a b
   . a
  -> b
  -> CTOR.Analyser a /\ b
analyser = Tuple <<< CTOR.Analyser

type CAnalyser a
  = CTOR.Analyser AnalyserNodeCb /\ a

------
-- | Make an audio worklet node.
-- |
audioWorkletNode
  :: forall sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions b
   . IsSymbol sym
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData AudioParameter
  => JSON.WriteForeign { | processorOptions }
  => AudioWorkletNodeResponse sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions
  -> AudioWorkletNodeOptions numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions
  -> b
  -> (CTOR.AudioWorkletNode sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions) /\ b
audioWorkletNode px nd b = Tuple (CTOR.AudioWorkletNode px nd) b

type CAudioWorkletNode sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions b
  = (CTOR.AudioWorkletNode sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions) /\ b

------
data Bandpass
  = Bandpass

instance convertBandpassFrequency :: Paramable a => ConvertOption Bandpass "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertBandpassQ :: Paramable a => ConvertOption Bandpass "q" a AudioParameter where
  convertOption _ _ = paramize

type BandpassOptional
  = (q :: AudioParameter)

type BandpassAll
  =
  ( freq :: AudioParameter
  | BandpassOptional
  )

defaultBandpass :: { | BandpassOptional }
defaultBandpass = { q: paramize 1.0 }

class BandpassCtor i bandpass | i -> bandpass where
  -- | Create a bandpass filter, connecting it to another unit
  -- |
  -- | ```purescript
  -- | bandpass { freq: 440.0 } { sinOsc: unit }
  -- | bandpass { freq: 440.0, q: 1.0 } { sinOsc: unit }
  -- | bandpass 440.0 { sinOsc: unit }
  -- | ```
  bandpass :: i -> bandpass

instance bandpassCtor1 ::
  ( ConvertOptionsWithDefaults Bandpass { | BandpassOptional } { | provided } { | BandpassAll }
  ) =>
  BandpassCtor { | provided } (b -> CTOR.Bandpass AudioParameter AudioParameter /\ b) where
  bandpass provided b = CTOR.Bandpass all.freq all.q /\ b
    where
    all :: { | BandpassAll }
    all = convertOptionsWithDefaults Bandpass defaultBandpass provided
else instance bandpassCtor2 :: Paramable a => BandpassCtor a (b -> CTOR.Bandpass AudioParameter AudioParameter /\ b) where
  bandpass a b = CTOR.Bandpass (paramize a) defaultBandpass.q /\ b

type CBandpass a
  = CTOR.Bandpass AudioParameter AudioParameter /\ a

------
data Constant
  = Constant

instance convertConstantFrequency :: Paramable a => ConvertOption Constant "offset" a AudioParameter where
  convertOption _ _ = paramize

instance convertConstantOnOff :: ConvertOption Constant "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertConstantAudioOnOff :: ConvertOption Constant "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type ConstantOptional
  = (onOff :: AudioOnOff)

type ConstantAll
  =
  ( offset :: AudioParameter
  | ConstantOptional
  )

defaultConstant :: { | ConstantOptional }
defaultConstant = { onOff: onOffIze _on }

class ConstantCtor i o | i -> o where
  -- | Make a constant value
  -- |
  -- | ```purescript
  -- | constant 0.5
  -- | ```
  constant :: i -> o

instance constantCtor1 ::
  ( ConvertOptionsWithDefaults Constant { | ConstantOptional } { | provided } { | ConstantAll }
  ) =>
  ConstantCtor { | provided } (CTOR.Constant AudioOnOff AudioParameter /\ {}) where
  constant provided = CTOR.Constant all.onOff all.offset /\ {}
    where
    all :: { | ConstantAll }
    all = convertOptionsWithDefaults Constant defaultConstant provided
else instance constantCtor2 :: Paramable a => ConstantCtor a (CTOR.Constant AudioOnOff AudioParameter /\ {}) where
  constant a = CTOR.Constant defaultConstant.onOff (paramize a) /\ {}

type CConstant
  = CTOR.Constant AudioOnOff AudioParameter /\ {}

------
-- | Make a convolver, aka reverb.
-- |
-- | ```purescript
-- | convolver (Proxy :: _ "room") (playBuf "track")
-- | ```
convolver
  :: forall s b
   . s
  -> b
  -> CTOR.Convolver s /\ b
convolver = Tuple <<< CTOR.Convolver

type CConvolver a b
  = CTOR.Convolver a /\ b

------
-- | Make a delay unit.
-- |
-- | ```purescript
-- | delay 0.5 (playBuf "track")
-- | ```
delay
  :: forall a b
   . Paramable a
  => a
  -> b
  -> CTOR.Delay AudioParameter /\ b
delay gvsv = Tuple (CTOR.Delay (paramize gvsv))

type CDelay a
  = CTOR.Delay AudioParameter /\ a

------
data DynamicsCompressor
  = DynamicsCompressor

instance convertDynamicsCompressorThreshold :: Paramable a => ConvertOption DynamicsCompressor "threshold" a AudioParameter where
  convertOption _ _ = paramize

instance convertDynamicsCompressorKnee :: Paramable a => ConvertOption DynamicsCompressor "knee" a AudioParameter where
  convertOption _ _ = paramize

instance convertDynamicsCompressorRatio :: Paramable a => ConvertOption DynamicsCompressor "ratio" a AudioParameter where
  convertOption _ _ = paramize

instance convertDynamicsCompressorAttack :: Paramable a => ConvertOption DynamicsCompressor "attack" a AudioParameter where
  convertOption _ _ = paramize

instance convertDynamicsCompressorRelease :: Paramable a => ConvertOption DynamicsCompressor "release" a AudioParameter where
  convertOption _ _ = paramize

type DynamicsCompressorOptional
  =
  ( threshold :: AudioParameter
  , knee :: AudioParameter
  , ratio :: AudioParameter
  , attack :: AudioParameter
  , release :: AudioParameter
  )

type DynamicsCompressorAll
  =
  (
  | DynamicsCompressorOptional
  )

defaultDynamicsCompressor :: { | DynamicsCompressorOptional }
defaultDynamicsCompressor =
  { threshold: paramize (-24.0)
  , knee: paramize 30.0
  , ratio: paramize 12.0
  , attack: paramize 0.003
  , release: paramize 0.25
  }

class DynamicsCompressorCtor i compressor | i -> compressor where
  -- | Make a compressor.
  -- |
  -- | ```purescript
  -- | compressor { threshold: -10.0 } { buf: playBuf "track" }
  -- | compressor { knee: 20.0, ratio: 10.0 } { buf: playBuf "track" }
  -- | compressor { attack: 0.01, release: 0.3 } { buf: playBuf "track" }
  -- | ```
  compressor :: i -> compressor

instance compressorCTor ::
  ( ConvertOptionsWithDefaults DynamicsCompressor { | DynamicsCompressorOptional } { | provided } { | DynamicsCompressorAll }
  ) =>
  DynamicsCompressorCtor { | provided } (b -> CTOR.DynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter /\ b) where
  compressor provided b =
    CTOR.DynamicsCompressor
      all.threshold
      all.knee
      all.ratio
      all.attack
      all.release
      /\ b
    where
    all :: { | DynamicsCompressorAll }
    all = convertOptionsWithDefaults DynamicsCompressor defaultDynamicsCompressor provided

type CDynamicsCompressor a
  = CTOR.DynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter /\ a

------
gain :: forall a b. Paramable a => a -> b -> CTOR.Gain AudioParameter /\ b
gain a = Tuple (CTOR.Gain (paramize a))

-- | Mix together several audio units
-- |
-- | ```purescript
-- | mix (playBuf (Proxy :: _ "hello") /\ playBuf (Proxy :: _ "world") /\ unit)
-- | ```
mix :: forall a. a -> CTOR.Gain AudioParameter /\ a
mix = Tuple (CTOR.Gain (paramize 1.0))

type Mix
  = CTOR.Gain AudioParameter

type CGain a
  = CTOR.Gain AudioParameter /\ a

------
data Highpass
  = Highpass

instance convertHighpassFrequency :: Paramable a => ConvertOption Highpass "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertHighpassQ :: Paramable a => ConvertOption Highpass "q" a AudioParameter where
  convertOption _ _ = paramize

type HighpassOptional
  = (q :: AudioParameter)

type HighpassAll
  =
  ( freq :: AudioParameter
  | HighpassOptional
  )

defaultHighpass :: { | HighpassOptional }
defaultHighpass = { q: paramize 1.0 }

class HighpassCtor i highpass | i -> highpass where
  -- | Make a highpass filter
  -- |
  -- | ```purescript
  -- | highpass { freq: 440.0 } { osc: sinOsc 440.0 }
  -- | highpass { freq: 440.0, q: 1.0 } { osc: sinOsc 440.0 }
  -- | highpass 440.0 { osc: sinOsc 440.0 }
  -- | ```
  highpass :: i -> highpass

instance highpassCtor1 ::
  ( ConvertOptionsWithDefaults Highpass { | HighpassOptional } { | provided } { | HighpassAll }
  ) =>
  HighpassCtor { | provided } (b -> CTOR.Highpass AudioParameter AudioParameter /\ b) where
  highpass provided = Tuple (CTOR.Highpass all.freq all.q)
    where
    all :: { | HighpassAll }
    all = convertOptionsWithDefaults Highpass defaultHighpass provided
else instance highpassCtor2 :: Paramable a => HighpassCtor a (b -> CTOR.Highpass AudioParameter AudioParameter /\ b) where
  highpass a = Tuple (CTOR.Highpass (paramize a) defaultHighpass.q)

type CHighpass a
  = CTOR.Highpass AudioParameter AudioParameter /\ a

------
data Highshelf
  = Highshelf

instance convertHighshelfFrequency :: Paramable a => ConvertOption Highshelf "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertHighshelfQ :: Paramable a => ConvertOption Highshelf "gain" a AudioParameter where
  convertOption _ _ = paramize

type HighshelfOptional
  = (gain :: AudioParameter)

type HighshelfAll
  =
  ( freq :: AudioParameter
  | HighshelfOptional
  )

defaultHighshelf :: { | HighshelfOptional }
defaultHighshelf = { gain: paramize 0.0 }

class HighshelfCtor i highshelf | i -> highshelf where
  -- | Make a highshelf filter
  -- |
  -- | ```purescript
  -- | highshelf { freq: 440.0 } { osc: sinOsc 440.0 }
  -- | highshelf { freq: 440.0, gain: 1.0 } { osc: sinOsc 440.0 }
  -- | highshelf 440.0 { osc: sinOsc 440.0 }
  -- | ```
  highshelf :: i -> highshelf

instance highshelfCtor1 ::
  ( ConvertOptionsWithDefaults Highshelf { | HighshelfOptional } { | provided } { | HighshelfAll }
  ) =>
  HighshelfCtor { | provided } (b -> CTOR.Highshelf AudioParameter AudioParameter /\ b) where
  highshelf provided = Tuple (CTOR.Highshelf all.freq all.gain)
    where
    all :: { | HighshelfAll }
    all = convertOptionsWithDefaults Highshelf defaultHighshelf provided
else instance highshelfCtor2 :: Paramable a => HighshelfCtor a (b -> CTOR.Highshelf AudioParameter AudioParameter /\ b) where
  highshelf a = Tuple (CTOR.Highshelf (paramize a) defaultHighshelf.gain)

type CHighshelf a
  = CTOR.Highshelf AudioParameter AudioParameter /\ a

----
-- | Make an input
-- |
-- | ```purescript
-- | input myInput
-- | ```
input
  :: forall proxy sym
   . IsSymbol sym
  => proxy sym
  -> CTOR.Input sym /\ {}
input _ = Tuple (CTOR.Input) {}

type CInput px
  = CTOR.Input px /\ {}

----
data LoopBuf
  = LoopBuf

instance convertLoopBufPlaybackRate :: Paramable a => ConvertOption LoopBuf "playbackRate" a AudioParameter where
  convertOption _ _ = paramize

instance convertLoopBufOnOff :: ConvertOption LoopBuf "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertLoopBufAudioOnOff :: ConvertOption LoopBuf "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

instance convertLoopBufStart :: ConvertOption LoopBuf "loopStart" Number Number where
  convertOption _ _ = identity

instance convertLoopBufEnd :: ConvertOption LoopBuf "loopEnd" Number Number where
  convertOption _ _ = identity

type LoopBufOptional
  = (playbackRate :: AudioParameter, onOff :: AudioOnOff, loopStart :: Number, loopEnd :: Number)

type LoopBufAll
  = (| LoopBufOptional)

defaultLoopBuf :: { | LoopBufOptional }
defaultLoopBuf = { playbackRate: paramize 1.0, onOff: onOffIze _on, loopStart: 0.0, loopEnd: 0.0 }

class LoopBufCtor i loopBuf | i -> loopBuf where
  -- | Make a looping buffer.
  -- |
  -- | ```purescript
  -- | loopBuf { playbackRate: 1.0 } "track"
  -- | loopBuf { playbackRate: 1.0, loopStart: 0.5 } "track"
  -- | loopBuf "track"
  -- | ```
  loopBuf :: i -> loopBuf

instance loopBufCtor1 ::
  ( ConvertOptionsWithDefaults LoopBuf { | LoopBufOptional } { | provided } { | LoopBufAll }
  ) =>
  LoopBufCtor { | provided } (BrowserAudioBuffer -> CTOR.LoopBuf BrowserAudioBuffer AudioOnOff AudioParameter Number Number /\ {}) where
  loopBuf provided proxy = CTOR.LoopBuf proxy all.onOff all.playbackRate all.loopStart all.loopEnd /\ {}
    where
    all :: { | LoopBufAll }
    all = convertOptionsWithDefaults LoopBuf defaultLoopBuf provided
else instance loopBufCtor2 :: LoopBufCtor BrowserAudioBuffer (CTOR.LoopBuf BrowserAudioBuffer AudioOnOff AudioParameter Number Number /\ {}) where
  loopBuf name =
    CTOR.LoopBuf
      name
      defaultLoopBuf.onOff
      defaultLoopBuf.playbackRate
      defaultLoopBuf.loopStart
      defaultLoopBuf.loopEnd
      /\ {}

type CLoopBuf
  = CTOR.LoopBuf BrowserAudioBuffer AudioOnOff AudioParameter Number Number /\ {}

-----
data Lowpass
  = Lowpass

instance convertLowpassFrequency :: Paramable a => ConvertOption Lowpass "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertLowpassQ :: Paramable a => ConvertOption Lowpass "q" a AudioParameter where
  convertOption _ _ = paramize

type LowpassOptional
  = (q :: AudioParameter)

type LowpassAll
  =
  ( freq :: AudioParameter
  | LowpassOptional
  )

defaultLowpass :: { | LowpassOptional }
defaultLowpass = { q: paramize 1.0 }

class LowpassCtor i lowpass | i -> lowpass where
  -- | Make a lowpass filter
  -- |
  -- | ```purescript
  -- | lowpass { freq: 440.0 } { osc: sinOsc 440.0 }
  -- | lowpass { freq: 440.0, q: 1.0 } { osc: sinOsc 440.0 }
  -- | lowpass 440.0 { osc: sinOsc 440.0 }
  -- | ```
  lowpass :: i -> lowpass

instance lowpassCtor1 ::
  ( ConvertOptionsWithDefaults Lowpass { | LowpassOptional } { | provided } { | LowpassAll }
  ) =>
  LowpassCtor { | provided } (b -> CTOR.Lowpass AudioParameter AudioParameter /\ b) where
  lowpass provided = Tuple (CTOR.Lowpass all.freq all.q)
    where
    all :: { | LowpassAll }
    all = convertOptionsWithDefaults Lowpass defaultLowpass provided
else instance lowpassCtor2 :: Paramable a => LowpassCtor a (b -> CTOR.Lowpass AudioParameter AudioParameter /\ b) where
  lowpass a = Tuple (CTOR.Lowpass (paramize a) defaultLowpass.q)

type CLowpass a
  = CTOR.Lowpass AudioParameter AudioParameter /\ a

-----
data Lowshelf
  = Lowshelf

instance convertLowshelfFrequency :: Paramable a => ConvertOption Lowshelf "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertLowshelfQ :: Paramable a => ConvertOption Lowshelf "gain" a AudioParameter where
  convertOption _ _ = paramize

type LowshelfOptional
  = (gain :: AudioParameter)

type LowshelfAll
  =
  ( freq :: AudioParameter
  | LowshelfOptional
  )

defaultLowshelf :: { | LowshelfOptional }
defaultLowshelf = { gain: paramize 0.0 }

class LowshelfCtor i lowshelf | i -> lowshelf where
  -- | Make a lowshelf filter
  -- |
  -- | ```purescript
  -- | lowshelf { freq: 440.0 } { osc: sinOsc 440.0 }
  -- | lowshelf { freq: 440.0, gain: 1.0 } { osc: sinOsc 440.0 }
  -- | lowshelf 440.0 { osc: sinOsc 440.0 }
  -- | ```
  lowshelf :: i -> lowshelf

instance lowshelfCtor1 ::
  ( ConvertOptionsWithDefaults Lowshelf { | LowshelfOptional } { | provided } { | LowshelfAll }
  ) =>
  LowshelfCtor { | provided } (b -> CTOR.Lowshelf AudioParameter AudioParameter /\ b) where
  lowshelf provided = Tuple (CTOR.Lowshelf all.freq all.gain)
    where
    all :: { | LowshelfAll }
    all = convertOptionsWithDefaults Lowshelf defaultLowshelf provided
else instance lowshelfCtor2 :: Paramable a => LowshelfCtor a (b -> CTOR.Lowshelf AudioParameter AudioParameter /\ b) where
  lowshelf a = Tuple (CTOR.Lowshelf (paramize a) defaultLowshelf.gain)

type CLowshelf a
  = CTOR.Lowshelf AudioParameter AudioParameter /\ a

--------

mediaElement :: BrowserMediaElement -> CTOR.MediaElement BrowserMediaElement /\ {}
mediaElement = flip (/\) {} <<< CTOR.MediaElement

type CMediaElement
  = CTOR.MediaElement BrowserMediaElement /\ {}
--------
microphone_ :: BrowserMicrophone -> { microphone :: CTOR.Microphone BrowserMicrophone /\ {} }
microphone_ = { microphone: _ } <<< microphone

microphone :: BrowserMicrophone -> CTOR.Microphone BrowserMicrophone /\ {}
microphone = flip (/\) {} <<< CTOR.Microphone

type CMicrophone
  = CTOR.Microphone BrowserMicrophone /\ {}

--------
data Notch
  = Notch

instance convertNotchFrequency :: Paramable a => ConvertOption Notch "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertNotchQ :: Paramable a => ConvertOption Notch "q" a AudioParameter where
  convertOption _ _ = paramize

type NotchOptional
  = (q :: AudioParameter)

type NotchAll
  =
  ( freq :: AudioParameter
  | NotchOptional
  )

defaultNotch :: { | NotchOptional }
defaultNotch = { q: paramize 1.0 }

class NotchCtor i notch | i -> notch where
  -- | Make a notch (band-reject) filter
  -- |
  -- | ```purescript
  -- | notch { freq: 440.0 } { osc: sinOsc 440.0 }
  -- | notch { freq: 440.0, gain: 1.0 } { osc: sinOsc 440.0 }
  -- | notch 440.0 { osc: sinOsc 440.0 }
  -- | ```
  notch :: i -> notch

instance notchCtor1 ::
  ( ConvertOptionsWithDefaults Notch { | NotchOptional } { | provided } { | NotchAll }
  ) =>
  NotchCtor { | provided } (b -> CTOR.Notch AudioParameter AudioParameter /\ b) where
  notch provided = Tuple (CTOR.Notch all.freq all.q)
    where
    all :: { | NotchAll }
    all = convertOptionsWithDefaults Notch defaultNotch provided
else instance notchCtor2 :: Paramable a => NotchCtor a (b -> CTOR.Notch AudioParameter AudioParameter /\ b) where
  notch a = Tuple (CTOR.Notch (paramize a) defaultNotch.q)

type CNotch a
  = CTOR.Notch AudioParameter AudioParameter /\ a

----------------
data Peaking
  = Peaking

instance convertPeakingFrequency :: Paramable a => ConvertOption Peaking "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertPeakingQ :: Paramable a => ConvertOption Peaking "q" a AudioParameter where
  convertOption _ _ = paramize

instance convertPeakingGain :: Paramable a => ConvertOption Peaking "gain" a AudioParameter where
  convertOption _ _ = paramize

type PeakingOptional
  = (q :: AudioParameter, gain :: AudioParameter)

type PeakingAll
  =
  ( freq :: AudioParameter
  | PeakingOptional
  )

defaultPeaking :: { | PeakingOptional }
defaultPeaking = { q: paramize 1.0, gain: paramize 0.0 }

class PeakingCtor i peaking | i -> peaking where
  -- | Make a peaking filter
  -- |
  -- | ```purescript
  -- | peaking { freq: 440.0 } { osc: sinOsc 440.0 }
  -- | peaking { freq: 440.0, gain: 1.0 } { osc: sinOsc 440.0 }
  -- | peaking 440.0 { osc: sinOsc 440.0 }
  -- | ```
  peaking :: i -> peaking

instance peakingCtor1 ::
  ( ConvertOptionsWithDefaults Peaking { | PeakingOptional } { | provided } { | PeakingAll }
  ) =>
  PeakingCtor { | provided } (b -> CTOR.Peaking AudioParameter AudioParameter AudioParameter /\ b) where
  peaking provided = Tuple (CTOR.Peaking all.freq all.q all.gain)
    where
    all :: { | PeakingAll }
    all = convertOptionsWithDefaults Peaking defaultPeaking provided
else instance peakingCtor2 :: Paramable a => PeakingCtor a (b -> CTOR.Peaking AudioParameter AudioParameter AudioParameter /\ b) where
  peaking a = Tuple (CTOR.Peaking (paramize a) defaultPeaking.q defaultPeaking.gain)

type CPeaking a
  = CTOR.Peaking AudioParameter AudioParameter AudioParameter /\ a

------
class CanBeCoercedToPeriodicOsc (canBeCoercedToPeriodicOsc :: Type)

instance canBeCoercedToPeriodicOscProxy :: CanBeCoercedToPeriodicOsc BrowserPeriodicWave

instance canBeCoercedToPeriodicOscV :: Lt D1 size => CanBeCoercedToPeriodicOsc (V.Vec size Number /\ V.Vec size Number)

data PeriodicOsc
  = PeriodicOsc

instance convertPeriodicOscFrequency :: Paramable a => ConvertOption PeriodicOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertPeriodicOscOnOff :: ConvertOption PeriodicOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertPeriodicOscAudioOnOff :: ConvertOption PeriodicOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

instance convertPeriodicOscWave :: CanBeCoercedToPeriodicOsc wave => ConvertOption PeriodicOsc "waveform" wave wave where
  convertOption _ _ = identity

type PeriodicOscOptional
  = (onOff :: AudioOnOff)

type PeriodicOscAll wave
  =
  ( freq :: AudioParameter
  , wave :: wave
  | PeriodicOscOptional
  )

defaultPeriodicOsc :: { | PeriodicOscOptional }
defaultPeriodicOsc = { onOff: onOffIze _on }

class PeriodicOscCtor i o | i -> o where
  -- | Make a periodicOsc value
  -- |
  -- | ```purescript
  -- | periodicOsc "my-osc" 0.5
  -- | ```
  periodicOsc :: i -> o

instance periodicOscCtor1 ::
  ( ConvertOptionsWithDefaults PeriodicOsc { | PeriodicOscOptional } { | provided } { | PeriodicOscAll wave }
  ) =>
  PeriodicOscCtor { | provided } (CTOR.PeriodicOsc wave AudioOnOff AudioParameter /\ {}) where
  periodicOsc provided = CTOR.PeriodicOsc all.wave all.onOff all.freq /\ {}
    where
    all :: { | PeriodicOscAll wave }
    all = convertOptionsWithDefaults PeriodicOsc defaultPeriodicOsc provided
else instance periodicOscCtor2 :: (CanBeCoercedToPeriodicOsc wave, Paramable a) => PeriodicOscCtor wave (a -> CTOR.PeriodicOsc wave AudioOnOff AudioParameter /\ {}) where
  periodicOsc wave a = CTOR.PeriodicOsc wave defaultPeriodicOsc.onOff (paramize a) /\ {}

type CPeriodicOsc periodicOsc
  = CTOR.PeriodicOsc periodicOsc AudioOnOff AudioParameter /\ {}

---
data PlayBuf
  = PlayBuf

instance convertPlayBufPlaybackRate :: Paramable a => ConvertOption PlayBuf "playbackRate" a AudioParameter where
  convertOption _ _ = paramize

instance convertPlayBufOnOff :: ConvertOption PlayBuf "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertPlayBufAudioOnOff :: ConvertOption PlayBuf "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

instance convertPlayBufAPBufferOffset :: ConvertOption PlayBuf "bufferOffset" Number Number where
  convertOption _ _ = identity

type PlayBufOptional
  = (playbackRate :: AudioParameter, onOff :: AudioOnOff, bufferOffset :: Number)

type PlayBufAll
  = (| PlayBufOptional)

defaultPlayBuf :: { | PlayBufOptional }
defaultPlayBuf = { playbackRate: paramize 1.0, onOff: onOffIze _on, bufferOffset: 0.0 }

class PlayBufCtor i playBuf | i -> playBuf where
  -- | Make a unit that plays from a buffer.
  -- |
  -- | ```purescript
  -- | playBuf { playbackRate: 1.0 } "track"
  -- | playBuf { playbackRate: 1.0, bufferOffset: 0.5 } "track"
  -- | playBuf "track"
  -- | ```
  playBuf :: i -> playBuf

instance playBufCtor1 ::
  ConvertOptionsWithDefaults PlayBuf { | PlayBufOptional } { | provided } { | PlayBufAll } =>
  PlayBufCtor { | provided } (BrowserAudioBuffer -> CTOR.PlayBuf BrowserAudioBuffer Number AudioOnOff AudioParameter /\ {}) where
  playBuf provided proxy = CTOR.PlayBuf proxy all.bufferOffset all.onOff all.playbackRate /\ {}
    where
    all :: { | PlayBufAll }
    all = convertOptionsWithDefaults PlayBuf defaultPlayBuf provided
else instance playBufCtor2 :: PlayBufCtor BrowserAudioBuffer (CTOR.PlayBuf BrowserAudioBuffer Number AudioOnOff AudioParameter /\ {}) where
  playBuf str =
    CTOR.PlayBuf
      str
      defaultPlayBuf.bufferOffset
      defaultPlayBuf.onOff
      defaultPlayBuf.playbackRate
      /\ {}

type CPlayBuf
  = CTOR.PlayBuf BrowserAudioBuffer Number AudioOnOff AudioParameter /\ {}

------
-- | Make a recorder.
-- |
-- | ```purescript
-- | recorder "track"
-- | ```
recorder
  :: forall a b
   . a
  -> b
  -> CTOR.Recorder a /\ b
recorder = Tuple <<< CTOR.Recorder

type CRecorder b
  = CTOR.Recorder MediaRecorderCb /\ b

------
data SawtoothOsc
  = SawtoothOsc

instance convertSawtoothOscFrequency :: Paramable a => ConvertOption SawtoothOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertSawtoothOscOnOff :: ConvertOption SawtoothOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertSawtoothOscAudioOnOff :: ConvertOption SawtoothOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type SawtoothOscOptional
  = (onOff :: AudioOnOff)

type SawtoothOscAll
  =
  ( freq :: AudioParameter
  | SawtoothOscOptional
  )

defaultSawtoothOsc :: { | SawtoothOscOptional }
defaultSawtoothOsc = { onOff: onOffIze _on }

class SawtoothOscCtor i o | i -> o where
  -- | Make a sawtoothOsc value
  -- |
  -- | ```purescript
  -- | sawtoothOsc 0.5
  -- | ```
  sawtoothOsc :: i -> o

instance sawtoothOscCtor1 ::
  ( ConvertOptionsWithDefaults SawtoothOsc { | SawtoothOscOptional } { | provided } { | SawtoothOscAll }
  ) =>
  SawtoothOscCtor { | provided } (CTOR.SawtoothOsc AudioOnOff AudioParameter /\ {}) where
  sawtoothOsc provided = CTOR.SawtoothOsc all.onOff all.freq /\ {}
    where
    all :: { | SawtoothOscAll }
    all = convertOptionsWithDefaults SawtoothOsc defaultSawtoothOsc provided
else instance sawtoothOscCtor2 :: Paramable a => SawtoothOscCtor a (CTOR.SawtoothOsc AudioOnOff AudioParameter /\ {}) where
  sawtoothOsc a = CTOR.SawtoothOsc defaultSawtoothOsc.onOff (paramize a) /\ {}

type CSawtoothOsc
  = CTOR.SawtoothOsc AudioOnOff AudioParameter /\ {}

------
data SinOsc
  = SinOsc

instance convertSinOscFrequency :: Paramable a => ConvertOption SinOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertSinOscOnOff :: ConvertOption SinOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertSinOscAudioOnOff :: ConvertOption SinOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type SinOscOptional
  = (onOff :: AudioOnOff)

type SinOscAll
  =
  ( freq :: AudioParameter
  | SinOscOptional
  )

defaultSinOsc :: { | SinOscOptional }
defaultSinOsc = { onOff: onOffIze _on }

class SinOscCtor i o | i -> o where
  -- | Make a sinOsc value
  -- |
  -- | ```purescript
  -- | sinOsc 0.5
  -- | ```
  sinOsc :: i -> o

instance sinOscCtor1 ::
  ( ConvertOptionsWithDefaults SinOsc { | SinOscOptional } { | provided } { | SinOscAll }
  ) =>
  SinOscCtor { | provided } (CTOR.SinOsc AudioOnOff AudioParameter /\ {}) where
  sinOsc provided = CTOR.SinOsc all.onOff all.freq /\ {}
    where
    all :: { | SinOscAll }
    all = convertOptionsWithDefaults SinOsc defaultSinOsc provided
else instance sinOscCtor2 :: Paramable a => SinOscCtor a (CTOR.SinOsc AudioOnOff AudioParameter /\ {}) where
  sinOsc a = CTOR.SinOsc defaultSinOsc.onOff (paramize a) /\ {}

type CSinOsc
  = CTOR.SinOsc AudioOnOff AudioParameter /\ {}

------
-- | Send sound to the loudspeaker.
-- |
-- | ```purescript
-- | speaker
-- | ```
speaker :: forall b. b -> { speaker :: CTOR.Speaker /\ b }
speaker b = { speaker: CTOR.Speaker /\ b }

-- | The raw constructor for speaker. Probably not useful...
speaker' :: forall b. b -> CTOR.Speaker /\ b
speaker' = Tuple CTOR.Speaker

type CSpeaker a
  = { speaker :: CTOR.Speaker /\ a }

------
data SquareOsc
  = SquareOsc

instance convertSquareOscFrequency :: Paramable a => ConvertOption SquareOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertSquareOscOnOff :: ConvertOption SquareOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertSquareOscAudioOnOff :: ConvertOption SquareOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type SquareOscOptional
  = (onOff :: AudioOnOff)

type SquareOscAll
  =
  ( freq :: AudioParameter
  | SquareOscOptional
  )

defaultSquareOsc :: { | SquareOscOptional }
defaultSquareOsc = { onOff: onOffIze _on }

class SquareOscCtor i o | i -> o where
  -- | Make a squareOsc value
  -- |
  -- | ```purescript
  -- | squareOsc 0.5
  -- | ```
  squareOsc :: i -> o

instance squareOscCtor1 ::
  ( ConvertOptionsWithDefaults SquareOsc { | SquareOscOptional } { | provided } { | SquareOscAll }
  ) =>
  SquareOscCtor { | provided } (CTOR.SquareOsc AudioOnOff AudioParameter /\ {}) where
  squareOsc provided = CTOR.SquareOsc all.onOff all.freq /\ {}
    where
    all :: { | SquareOscAll }
    all = convertOptionsWithDefaults SquareOsc defaultSquareOsc provided
else instance squareOscCtor2 :: Paramable a => SquareOscCtor a (CTOR.SquareOsc AudioOnOff AudioParameter /\ {}) where
  squareOsc a = CTOR.SquareOsc defaultSquareOsc.onOff (paramize a) /\ {}

type CSquareOsc
  = CTOR.SquareOsc AudioOnOff AudioParameter /\ {}

------
-- | Pan audio.
-- |
-- | ```purescript
-- | pan 0.5 { buf: playBuf "my-track" }
-- | ```
pan
  :: forall a b
   . Paramable a
  => a
  -> b
  -> CTOR.StereoPanner AudioParameter /\ b
pan gvsv = Tuple (CTOR.StereoPanner (paramize gvsv))

type CStereoPanner a
  = CTOR.StereoPanner AudioParameter /\ a

----
-- | Make a subgraph
-- |
-- | ```purescript
-- | input myInput
-- | ```
-- | the validity of inputs with respect to r is validated higher upstream (at the scene construction level)
subgraph
  :: forall n inputs info terminus env r
   . Pos n
  => V.Vec n info
  -> ( forall audio engine
        . AudioInterpret audio engine
       => Int
       -> info
       -> SubScene terminus inputs env audio engine Frame0 Unit
     )
  -> (Int -> info -> env)
  -> r
  -> (CTOR.Subgraph inputs (V.Vec n info) (AsSubgraph terminus inputs info env) (Int -> info -> env)) /\ r
subgraph vec sg ev = Tuple (CTOR.Subgraph vec (AsSubgraph sg) ev)

subgraphSetter
  :: forall n inputs info env
   . Pos n
  => V.Vec n info
  -> (Int -> info -> env)
  -> (CTOR.Subgraph inputs (V.Vec n info) Unit (Int -> info -> env))
subgraphSetter vec ev = CTOR.Subgraph vec unit ev

subgraphSingleSetter
  :: forall n env
   . Nat n
  => n
  -> env
  -> ChangeSubgraph n env
subgraphSingleSetter c ev = ChangeSubgraph (c /\ ev)

type CSubgraph (n :: Type) info terminus inputs env r
  = (CTOR.Subgraph inputs (V.Vec n info) (AsSubgraph terminus inputs info env) (Int -> info -> env)) /\ r

------
data TriangleOsc
  = TriangleOsc

instance convertTriangleOscFrequency :: Paramable a => ConvertOption TriangleOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertTriangleOscOnOff :: ConvertOption TriangleOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertTriangleOscAudioOnOff :: ConvertOption TriangleOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type TriangleOscOptional
  = (onOff :: AudioOnOff)

type TriangleOscAll
  =
  ( freq :: AudioParameter
  | TriangleOscOptional
  )

defaultTriangleOsc :: { | TriangleOscOptional }
defaultTriangleOsc = { onOff: onOffIze _on }

class TriangleOscCtor i o | i -> o where
  -- | Make a triangleOsc value
  -- |
  -- | ```purescript
  -- | triangleOsc 0.5
  -- | ```
  triangleOsc :: i -> o

instance triangleOscCtor1 ::
  ( ConvertOptionsWithDefaults TriangleOsc { | TriangleOscOptional } { | provided } { | TriangleOscAll }
  ) =>
  TriangleOscCtor { | provided } (CTOR.TriangleOsc AudioOnOff AudioParameter /\ {}) where
  triangleOsc provided = CTOR.TriangleOsc all.onOff all.freq /\ {}
    where
    all :: { | TriangleOscAll }
    all = convertOptionsWithDefaults TriangleOsc defaultTriangleOsc provided
else instance triangleOscCtor2 :: Paramable a => TriangleOscCtor a (CTOR.TriangleOsc AudioOnOff AudioParameter /\ {}) where
  triangleOsc a = CTOR.TriangleOsc defaultTriangleOsc.onOff (paramize a) /\ {}

type CTriangleOsc
  = CTOR.TriangleOsc AudioOnOff AudioParameter /\ {}

--------
----
-- | Make tumult
-- |
-- | the validity of inputs with respect to r is validated higher upstream (at the scene construction level)
tumult
  :: forall n terminus inputs r
   . Pos n
  => Tumultuous n terminus inputs
  -> r
  -> (CTOR.Tumult (Tumultuous n terminus inputs)) /\ r
tumult tmt = Tuple (CTOR.Tumult tmt)

type CTumult :: forall k1 k2. Type -> k1 -> k2 -> Type -> Type
type CTumult (n :: Type) terminus inputs r
  = (CTOR.Tumult (Tumultuous n terminus inputs)) /\ r

----------
-- | Apply distorion to audio
-- |
-- | ```purescript
-- | waveShaper (Proxy :: _ "my-wave") OversampleNone { buf: playBuf "my-track" }
-- | ```
waveShaper
  :: forall a b c
   . IsOversample b
  => a
  -> b
  -> c
  -> CTOR.WaveShaper a b /\ c
waveShaper a = Tuple <<< CTOR.WaveShaper a

type CWaveShaper b c
  = CTOR.WaveShaper BrowserFloatArray b /\ c

---------------
-- | A reference to a node in a graph.
type Ref
  = Unit /\ {}

-- | A reference to a node in a graph.
ref :: Ref
ref = unit /\ {}
