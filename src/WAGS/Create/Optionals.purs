-- | This module provides functions for the construction of audio units that more closely resemble the overloaded constructors of the Web Audio API.
module WAGS.Create.Optionals where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Hashable (class Hashable, hash)
import Data.Map (Map, toUnfoldable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D1)
import Data.Variant.Maybe (Maybe)
import Data.Vec as V
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import WAGS.Control.Types (Frame0, SubScene)
import WAGS.Graph.AudioUnit (AudioWorkletNodeOptions)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Oversample (class IsOversample)
import WAGS.Graph.Paramable (class Paramable, onOffIze, paramize)
import WAGS.Graph.Parameter (AudioOnOff, OnOff, _on, AudioParameter)
import WAGS.Graph.Worklet (AudioWorkletNodeResponse)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Tumult (Tumultuous)
import WAGS.Util (class ValidateOutputChannelCount)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

-----------
data Allpass = Allpass

instance convertAllpassFrequency ::
  Paramable a =>
  ConvertOption Allpass "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertAllpassQ ::
  Paramable a =>
  ConvertOption Allpass "q" a AudioParameter where
  convertOption _ _ = paramize

type AllpassOptional = (q :: AudioParameter)

type AllpassAll =
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
  ( ConvertOptionsWithDefaults Allpass { | AllpassOptional } { | provided }
      { | AllpassAll }
  ) =>
  AllpassCtor { | provided } (b -> CTOR.Allpass /\ b) where
  allpass provided b = CTOR.Allpass all /\ b
    where
    all :: { | AllpassAll }
    all = convertOptionsWithDefaults Allpass defaultAllpass provided
else instance allpassCtor2 ::
  Paramable a =>
  AllpassCtor a (b -> CTOR.Allpass /\ b) where
  allpass a b = CTOR.Allpass { freq: paramize a, q: defaultAllpass.q } /\ b

type CAllpass a = CTOR.Allpass /\ a

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

type CAnalyser a = CTOR.Analyser AnalyserNodeCb /\ a

------
-- | Make an audio worklet node.
-- |
audioWorkletNode
  :: forall sym numberOfInputs numberOfOutputs outputChannelCount parameterData
       processorOptions b
   . IsSymbol sym
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData AudioParameter
  => JSON.WriteForeign { | processorOptions }
  => AudioWorkletNodeResponse sym numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> AudioWorkletNodeOptions numberOfInputs numberOfOutputs outputChannelCount
       parameterData
       processorOptions
  -> b
  -> ( CTOR.AudioWorkletNode sym numberOfInputs numberOfOutputs
         outputChannelCount
         parameterData
         processorOptions
     ) /\ b
audioWorkletNode px nd b = Tuple (CTOR.AudioWorkletNode px nd) b

type CAudioWorkletNode
  sym
  numberOfInputs
  numberOfOutputs
  outputChannelCount
  parameterData
  processorOptions
  b =
  ( CTOR.AudioWorkletNode sym numberOfInputs numberOfOutputs outputChannelCount
      parameterData
      processorOptions
  ) /\ b

------
data Bandpass = Bandpass

instance convertBandpassFrequency ::
  Paramable a =>
  ConvertOption Bandpass "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertBandpassQ ::
  Paramable a =>
  ConvertOption Bandpass "q" a AudioParameter where
  convertOption _ _ = paramize

type BandpassOptional = (q :: AudioParameter)

type BandpassAll =
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
  ( ConvertOptionsWithDefaults Bandpass { | BandpassOptional } { | provided }
      { | BandpassAll }
  ) =>
  BandpassCtor { | provided } (b -> CTOR.Bandpass /\ b) where
  bandpass provided b = CTOR.Bandpass all /\ b
    where
    all = convertOptionsWithDefaults Bandpass defaultBandpass provided
else instance bandpassCtor2 ::
  Paramable a =>
  BandpassCtor a (b -> CTOR.Bandpass /\ b) where
  bandpass a b = CTOR.Bandpass { freq: paramize a, q: defaultBandpass.q } /\ b

type CBandpass a = CTOR.Bandpass /\ a

------
data Constant = Constant

instance convertConstantFrequency ::
  Paramable a =>
  ConvertOption Constant "offset" a AudioParameter where
  convertOption _ _ = paramize

instance convertConstantOnOff :: ConvertOption Constant "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertConstantAudioOnOff ::
  ConvertOption Constant "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type ConstantOptional = (onOff :: AudioOnOff)

type ConstantAll =
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
  ( ConvertOptionsWithDefaults Constant { | ConstantOptional } { | provided }
      { | ConstantAll }
  ) =>
  ConstantCtor { | provided } (CTOR.Constant /\ {}) where
  constant provided = CTOR.Constant all /\ {}
    where
    all :: { | ConstantAll }
    all = convertOptionsWithDefaults Constant defaultConstant provided
else instance constantCtor2 ::
  Paramable a =>
  ConstantCtor a (CTOR.Constant /\ {}) where
  constant a =
    CTOR.Constant { onOff: defaultConstant.onOff, offset: paramize a } /\ {}

type CConstant = CTOR.Constant /\ {}

------
-- | Make a convolver, aka reverb.
-- |
-- | ```purescript
-- | convolver (Proxy :: _ "room") (playBuf "track")
-- | ```
convolver
  :: forall b
   . BrowserAudioBuffer
  -> b
  -> CTOR.Convolver /\ b
convolver = Tuple <<< CTOR.Convolver <<< { buffer: _ }

type CConvolver b = CTOR.Convolver /\ b

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
  -> CTOR.Delay /\ b
delay gvsv = Tuple (CTOR.Delay { delayTime: paramize gvsv })

type CDelay a = CTOR.Delay /\ a

------
data DynamicsCompressor = DynamicsCompressor

instance convertDynamicsCompressorThreshold ::
  Paramable a =>
  ConvertOption DynamicsCompressor "threshold" a AudioParameter where
  convertOption _ _ = paramize

instance convertDynamicsCompressorKnee ::
  Paramable a =>
  ConvertOption DynamicsCompressor "knee" a AudioParameter where
  convertOption _ _ = paramize

instance convertDynamicsCompressorRatio ::
  Paramable a =>
  ConvertOption DynamicsCompressor "ratio" a AudioParameter where
  convertOption _ _ = paramize

instance convertDynamicsCompressorAttack ::
  Paramable a =>
  ConvertOption DynamicsCompressor "attack" a AudioParameter where
  convertOption _ _ = paramize

instance convertDynamicsCompressorRelease ::
  Paramable a =>
  ConvertOption DynamicsCompressor "release" a AudioParameter where
  convertOption _ _ = paramize

type DynamicsCompressorOptional =
  ( threshold :: AudioParameter
  , knee :: AudioParameter
  , ratio :: AudioParameter
  , attack :: AudioParameter
  , release :: AudioParameter
  )

type DynamicsCompressorAll =
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
  ( ConvertOptionsWithDefaults DynamicsCompressor
      { | DynamicsCompressorOptional }
      { | provided }
      { | DynamicsCompressorAll }
  ) =>
  DynamicsCompressorCtor { | provided } (b -> CTOR.DynamicsCompressor /\ b) where
  compressor provided b = CTOR.DynamicsCompressor all /\ b
    where
    all :: { | DynamicsCompressorAll }
    all = convertOptionsWithDefaults DynamicsCompressor
      defaultDynamicsCompressor
      provided

type CDynamicsCompressor a = CTOR.DynamicsCompressor /\ a

------
gain :: forall a b. Paramable a => a -> b -> CTOR.Gain /\ b
gain a = Tuple (CTOR.Gain { gain: paramize a })

-- | Mix together several audio units
-- |
-- | ```purescript
-- | mix (playBuf (Proxy :: _ "hello") /\ playBuf (Proxy :: _ "world") /\ unit)
-- | ```
mix :: forall a. a -> CTOR.Gain /\ a
mix = Tuple (CTOR.Gain { gain: paramize 1.0 })

type Mix = CTOR.Gain

type CGain a = CTOR.Gain /\ a

------
data Highpass = Highpass

instance convertHighpassFrequency ::
  Paramable a =>
  ConvertOption Highpass "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertHighpassQ ::
  Paramable a =>
  ConvertOption Highpass "q" a AudioParameter where
  convertOption _ _ = paramize

type HighpassOptional = (q :: AudioParameter)

type HighpassAll =
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
  ( ConvertOptionsWithDefaults Highpass { | HighpassOptional } { | provided }
      { | HighpassAll }
  ) =>
  HighpassCtor { | provided } (b -> CTOR.Highpass /\ b) where
  highpass provided = Tuple (CTOR.Highpass all)
    where
    all :: { | HighpassAll }
    all = convertOptionsWithDefaults Highpass defaultHighpass provided
else instance highpassCtor2 ::
  Paramable a =>
  HighpassCtor a (b -> CTOR.Highpass /\ b) where
  highpass a = Tuple (CTOR.Highpass { freq: paramize a, q: defaultHighpass.q })

type CHighpass a = CTOR.Highpass /\ a

------
data Highshelf = Highshelf

instance convertHighshelfFrequency ::
  Paramable a =>
  ConvertOption Highshelf "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertHighshelfQ ::
  Paramable a =>
  ConvertOption Highshelf "gain" a AudioParameter where
  convertOption _ _ = paramize

type HighshelfOptional = (gain :: AudioParameter)

type HighshelfAll =
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
  ( ConvertOptionsWithDefaults Highshelf { | HighshelfOptional } { | provided }
      { | HighshelfAll }
  ) =>
  HighshelfCtor { | provided } (b -> CTOR.Highshelf /\ b) where
  highshelf provided = Tuple (CTOR.Highshelf all)
    where
    all :: { | HighshelfAll }
    all = convertOptionsWithDefaults Highshelf defaultHighshelf provided
else instance highshelfCtor2 ::
  Paramable a =>
  HighshelfCtor a (b -> CTOR.Highshelf /\ b) where
  highshelf a = Tuple
    (CTOR.Highshelf { freq: paramize a, gain: defaultHighshelf.gain })

type CHighshelf a = CTOR.Highshelf /\ a

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

type CInput px = CTOR.Input px /\ {}

----
data LoopBuf = LoopBuf

instance convertLoopBufPlaybackRate ::
  Paramable a =>
  ConvertOption LoopBuf "playbackRate" a AudioParameter where
  convertOption _ _ = paramize

instance convertLoopBufOnOff :: ConvertOption LoopBuf "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertLoopBufAudioOnOff ::
  ConvertOption LoopBuf "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

instance convertLoopBufStart :: ConvertOption LoopBuf "loopStart" Number Number where
  convertOption _ _ = identity

instance convertLoopBufEnd :: ConvertOption LoopBuf "loopEnd" Number Number where
  convertOption _ _ = identity

type LoopBufOptional =
  ( playbackRate :: AudioParameter
  , onOff :: AudioOnOff
  , loopStart :: Number
  , loopEnd :: Number
  )

type LoopBufAll = (| LoopBufOptional)

defaultLoopBuf :: { | LoopBufOptional }
defaultLoopBuf =
  { playbackRate: paramize 1.0
  , onOff: onOffIze _on
  , loopStart: 0.0
  , loopEnd: 0.0
  }

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
  ( ConvertOptionsWithDefaults LoopBuf { | LoopBufOptional } { | provided }
      { | LoopBufAll }
  ) =>
  LoopBufCtor { | provided } (BrowserAudioBuffer -> CTOR.LoopBuf /\ {}) where
  loopBuf provided buffer =
    CTOR.LoopBuf
      { buffer
      , onOff: all.onOff
      , playbackRate: all.playbackRate
      , loopStart: all.loopStart
      , loopEnd: all.loopEnd
      } /\ {}
    where
    all :: { | LoopBufAll }
    all = convertOptionsWithDefaults LoopBuf defaultLoopBuf provided
else instance loopBufCtor2 ::
  LoopBufCtor BrowserAudioBuffer (CTOR.LoopBuf /\ {}) where
  loopBuf buffer =
    CTOR.LoopBuf
      { buffer
      , onOff: defaultLoopBuf.onOff
      , playbackRate: defaultLoopBuf.playbackRate
      , loopStart: defaultLoopBuf.loopStart
      , loopEnd: defaultLoopBuf.loopEnd
      }
      /\ {}

type CLoopBuf = CTOR.LoopBuf /\ {}

-----
data Lowpass = Lowpass

instance convertLowpassFrequency ::
  Paramable a =>
  ConvertOption Lowpass "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertLowpassQ ::
  Paramable a =>
  ConvertOption Lowpass "q" a AudioParameter where
  convertOption _ _ = paramize

type LowpassOptional = (q :: AudioParameter)

type LowpassAll =
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
  ( ConvertOptionsWithDefaults Lowpass { | LowpassOptional } { | provided }
      { | LowpassAll }
  ) =>
  LowpassCtor { | provided } (b -> CTOR.Lowpass /\ b) where
  lowpass provided = Tuple (CTOR.Lowpass all)
    where
    all :: { | LowpassAll }
    all = convertOptionsWithDefaults Lowpass defaultLowpass provided
else instance lowpassCtor2 ::
  Paramable a =>
  LowpassCtor a (b -> CTOR.Lowpass /\ b) where
  lowpass a = Tuple
    ( CTOR.Lowpass
        { freq: paramize a
        , q: defaultLowpass.q
        }
    )

type CLowpass a = CTOR.Lowpass /\ a

-----
data Lowshelf = Lowshelf

instance convertLowshelfFrequency ::
  Paramable a =>
  ConvertOption Lowshelf "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertLowshelfQ ::
  Paramable a =>
  ConvertOption Lowshelf "gain" a AudioParameter where
  convertOption _ _ = paramize

type LowshelfOptional = (gain :: AudioParameter)

type LowshelfAll =
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
  ( ConvertOptionsWithDefaults Lowshelf { | LowshelfOptional } { | provided }
      { | LowshelfAll }
  ) =>
  LowshelfCtor { | provided } (b -> CTOR.Lowshelf /\ b) where
  lowshelf provided = Tuple (CTOR.Lowshelf all)
    where
    all :: { | LowshelfAll }
    all = convertOptionsWithDefaults Lowshelf defaultLowshelf provided
else instance lowshelfCtor2 ::
  Paramable a =>
  LowshelfCtor a (b -> CTOR.Lowshelf /\ b) where
  lowshelf a = Tuple
    (CTOR.Lowshelf { freq: paramize a, gain: defaultLowshelf.gain })

type CLowshelf a = CTOR.Lowshelf /\ a

--------

mediaElement :: BrowserMediaElement -> CTOR.MediaElement /\ {}
mediaElement = flip (/\) {} <<< CTOR.MediaElement <<< { media: _ }

type CMediaElement = CTOR.MediaElement /\ {}

--------
microphone_ :: BrowserMicrophone -> { microphone :: CTOR.Microphone /\ {} }
microphone_ = { microphone: _ } <<< microphone

microphone :: BrowserMicrophone -> CTOR.Microphone /\ {}
microphone = flip (/\) {} <<< CTOR.Microphone <<< { microphone: _ }

type CMicrophone = CTOR.Microphone /\ {}

--------
data Notch = Notch

instance convertNotchFrequency ::
  Paramable a =>
  ConvertOption Notch "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertNotchQ ::
  Paramable a =>
  ConvertOption Notch "q" a AudioParameter where
  convertOption _ _ = paramize

type NotchOptional = (q :: AudioParameter)

type NotchAll =
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
  ( ConvertOptionsWithDefaults Notch { | NotchOptional } { | provided }
      { | NotchAll }
  ) =>
  NotchCtor { | provided } (b -> CTOR.Notch /\ b) where
  notch provided = Tuple (CTOR.Notch all)
    where
    all :: { | NotchAll }
    all = convertOptionsWithDefaults Notch defaultNotch provided
else instance notchCtor2 :: Paramable a => NotchCtor a (b -> CTOR.Notch /\ b) where
  notch a = Tuple (CTOR.Notch { freq: paramize a, q: defaultNotch.q })

type CNotch a = CTOR.Notch /\ a

----------------
data Peaking = Peaking

instance convertPeakingFrequency ::
  Paramable a =>
  ConvertOption Peaking "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertPeakingQ ::
  Paramable a =>
  ConvertOption Peaking "q" a AudioParameter where
  convertOption _ _ = paramize

instance convertPeakingGain ::
  Paramable a =>
  ConvertOption Peaking "gain" a AudioParameter where
  convertOption _ _ = paramize

type PeakingOptional = (q :: AudioParameter, gain :: AudioParameter)

type PeakingAll =
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
  ( ConvertOptionsWithDefaults Peaking { | PeakingOptional } { | provided }
      { | PeakingAll }
  ) =>
  PeakingCtor { | provided } (b -> CTOR.Peaking /\ b) where
  peaking provided = Tuple (CTOR.Peaking all)
    where
    all :: { | PeakingAll }
    all = convertOptionsWithDefaults Peaking defaultPeaking provided
else instance peakingCtor2 ::
  Paramable a =>
  PeakingCtor a (b -> CTOR.Peaking /\ b) where
  peaking a = Tuple
    ( CTOR.Peaking
        { freq: paramize a, q: defaultPeaking.q, gain: defaultPeaking.gain }
    )

type CPeaking a = CTOR.Peaking /\ a

------
class CanBeCoercedToPeriodicOsc (canBeCoercedToPeriodicOsc :: Type)

instance canBeCoercedToPeriodicOscProxy ::
  CanBeCoercedToPeriodicOsc BrowserPeriodicWave

instance canBeCoercedToPeriodicOscV ::
  Lt D1 size =>
  CanBeCoercedToPeriodicOsc (V.Vec size Number /\ V.Vec size Number)

data PeriodicOsc = PeriodicOsc

instance convertPeriodicOscFrequency ::
  Paramable a =>
  ConvertOption PeriodicOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertPeriodicOscOnOff ::
  ConvertOption PeriodicOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertPeriodicOscAudioOnOff ::
  ConvertOption PeriodicOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

instance convertPeriodicOscWave ::
  CanBeCoercedToPeriodicOsc wave =>
  ConvertOption PeriodicOsc "waveform" wave wave where
  convertOption _ _ = identity

type PeriodicOscOptional = (onOff :: AudioOnOff)

type PeriodicOscAll wave =
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
  ( ConvertOptionsWithDefaults PeriodicOsc { | PeriodicOscOptional }
      { | provided }
      { | PeriodicOscAll wave }
  ) =>
  PeriodicOscCtor { | provided } (CTOR.PeriodicOsc wave /\ {}) where
  periodicOsc provided = CTOR.PeriodicOsc all /\ {}
    where
    all :: { | PeriodicOscAll wave }
    all = convertOptionsWithDefaults PeriodicOsc defaultPeriodicOsc provided
else instance periodicOscCtor2 ::
  ( CanBeCoercedToPeriodicOsc wave
  , Paramable a
  ) =>
  PeriodicOscCtor wave (a -> CTOR.PeriodicOsc wave /\ {}) where
  periodicOsc wave a =
    CTOR.PeriodicOsc { wave, onOff: defaultPeriodicOsc.onOff, freq: paramize a }
      /\ {}

type CPeriodicOsc periodicOsc = CTOR.PeriodicOsc periodicOsc /\ {}

---
data PlayBuf = PlayBuf

instance convertPlayBufPlaybackRate ::
  Paramable a =>
  ConvertOption PlayBuf "playbackRate" a AudioParameter where
  convertOption _ _ = paramize

instance convertPlayBufOnOff :: ConvertOption PlayBuf "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertPlayBufAudioOnOff ::
  ConvertOption PlayBuf "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

instance convertPlayBufAPBufferOffset ::
  ConvertOption PlayBuf "bufferOffset" Number Number where
  convertOption _ _ = identity

type PlayBufOptional =
  (playbackRate :: AudioParameter, onOff :: AudioOnOff, bufferOffset :: Number)

type PlayBufAll = (| PlayBufOptional)

defaultPlayBuf :: { | PlayBufOptional }
defaultPlayBuf =
  { playbackRate: paramize 1.0, onOff: onOffIze _on, bufferOffset: 0.0 }

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
  ConvertOptionsWithDefaults PlayBuf { | PlayBufOptional } { | provided }
    { | PlayBufAll } =>
  PlayBufCtor { | provided } (BrowserAudioBuffer -> CTOR.PlayBuf /\ {}) where
  playBuf provided buffer =
    CTOR.PlayBuf
      { buffer
      , bufferOffset: all.bufferOffset
      , onOff: all.onOff
      , playbackRate: all.playbackRate
      } /\ {}
    where
    all :: { | PlayBufAll }
    all = convertOptionsWithDefaults PlayBuf defaultPlayBuf provided
else instance playBufCtor2 ::
  PlayBufCtor BrowserAudioBuffer (CTOR.PlayBuf /\ {}) where
  playBuf buffer =
    CTOR.PlayBuf
      { buffer
      , bufferOffset: defaultPlayBuf.bufferOffset
      , onOff: defaultPlayBuf.onOff
      , playbackRate: defaultPlayBuf.playbackRate
      }
      /\ {}

type CPlayBuf = CTOR.PlayBuf /\ {}

------
-- | Make a recorder.
-- |
-- | ```purescript
-- | recorder "track"
-- | ```
recorder
  :: forall b
   . MediaRecorderCb
  -> b
  -> CTOR.Recorder /\ b
recorder = Tuple <<< CTOR.Recorder <<< { cb: _ }

type CRecorder b = CTOR.Recorder /\ b

------
data SawtoothOsc = SawtoothOsc

instance convertSawtoothOscFrequency ::
  Paramable a =>
  ConvertOption SawtoothOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertSawtoothOscOnOff ::
  ConvertOption SawtoothOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertSawtoothOscAudioOnOff ::
  ConvertOption SawtoothOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type SawtoothOscOptional = (onOff :: AudioOnOff)

type SawtoothOscAll =
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
  ( ConvertOptionsWithDefaults SawtoothOsc { | SawtoothOscOptional }
      { | provided }
      { | SawtoothOscAll }
  ) =>
  SawtoothOscCtor { | provided } (CTOR.SawtoothOsc /\ {}) where
  sawtoothOsc provided = CTOR.SawtoothOsc all /\ {}
    where
    all :: { | SawtoothOscAll }
    all = convertOptionsWithDefaults SawtoothOsc defaultSawtoothOsc provided
else instance sawtoothOscCtor2 ::
  Paramable a =>
  SawtoothOscCtor a (CTOR.SawtoothOsc /\ {}) where
  sawtoothOsc a =
    CTOR.SawtoothOsc { onOff: defaultSawtoothOsc.onOff, freq: paramize a } /\ {}

type CSawtoothOsc = CTOR.SawtoothOsc /\ {}

------
data SinOsc = SinOsc

instance convertSinOscFrequency ::
  Paramable a =>
  ConvertOption SinOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertSinOscOnOff :: ConvertOption SinOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertSinOscAudioOnOff ::
  ConvertOption SinOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type SinOscOptional = (onOff :: AudioOnOff)

type SinOscAll =
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
  ( ConvertOptionsWithDefaults SinOsc { | SinOscOptional } { | provided }
      { | SinOscAll }
  ) =>
  SinOscCtor { | provided } (CTOR.SinOsc /\ {}) where
  sinOsc provided = CTOR.SinOsc all /\ {}
    where
    all :: { | SinOscAll }
    all = convertOptionsWithDefaults SinOsc defaultSinOsc provided
else instance sinOscCtor2 :: Paramable a => SinOscCtor a (CTOR.SinOsc /\ {}) where
  sinOsc a = CTOR.SinOsc { onOff: defaultSinOsc.onOff, freq: paramize a } /\ {}

type CSinOsc = CTOR.SinOsc /\ {}

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

type CSpeaker a = { speaker :: CTOR.Speaker /\ a }

------
data SquareOsc = SquareOsc

instance convertSquareOscFrequency ::
  Paramable a =>
  ConvertOption SquareOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertSquareOscOnOff ::
  ConvertOption SquareOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertSquareOscAudioOnOff ::
  ConvertOption SquareOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type SquareOscOptional = (onOff :: AudioOnOff)

type SquareOscAll =
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
  ( ConvertOptionsWithDefaults SquareOsc { | SquareOscOptional } { | provided }
      { | SquareOscAll }
  ) =>
  SquareOscCtor { | provided } (CTOR.SquareOsc /\ {}) where
  squareOsc provided = CTOR.SquareOsc all /\ {}
    where
    all :: { | SquareOscAll }
    all = convertOptionsWithDefaults SquareOsc defaultSquareOsc provided
else instance squareOscCtor2 ::
  Paramable a =>
  SquareOscCtor a (CTOR.SquareOsc /\ {}) where
  squareOsc a =
    CTOR.SquareOsc { onOff: defaultSquareOsc.onOff, freq: (paramize a) } /\ {}

type CSquareOsc = CTOR.SquareOsc /\ {}

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
  -> CTOR.StereoPanner /\ b
pan = Tuple <<< CTOR.StereoPanner <<< { pan: _ } <<< paramize

type CStereoPanner a = CTOR.StereoPanner /\ a

----
-- | Make a subgraph
-- |
-- | ```purescript
-- | input myInput
-- | ```
-- | the validity of inputs with respect to r is validated higher upstream (at the scene construction level)
subgraph
  :: forall index inputs terminus env r
   . Hashable index
  => IsSymbol terminus
  => Map index (Maybe env)
  -> ( forall audio engine
        . AudioInterpret audio engine
       => index
       -> SubScene terminus inputs env audio engine Frame0 Unit
     )
  -> r
  -> (CTOR.Subgraph terminus inputs index env) /\ r
subgraph envs sg = Tuple
  ( CTOR.Subgraph
      { subgraphMaker: (CTOR.AsSubgraph sg)
      , envs: map (\(index /\ env) -> { index, pos: hash index, env })
          (toUnfoldable envs)
      , terminus: reflectSymbol (Proxy :: _ terminus)
      }
  )

subgraphSetter
  :: forall index env
   . Hashable index
  => Map index (Maybe env)
  -> CTOR.XSubgraph index env
subgraphSetter envs = CTOR.XSubgraph
  { envs: map (\(index /\ env) -> { index, pos: hash index, env })
      (toUnfoldable envs)
  }

type CSubgraph terminus inputs index env r =
  (CTOR.Subgraph terminus inputs index env) /\ r

------
data TriangleOsc = TriangleOsc

instance convertTriangleOscFrequency ::
  Paramable a =>
  ConvertOption TriangleOsc "freq" a AudioParameter where
  convertOption _ _ = paramize

instance convertTriangleOscOnOff ::
  ConvertOption TriangleOsc "onOff" OnOff AudioOnOff where
  convertOption _ _ = onOffIze

instance convertTriangleOscAudioOnOff ::
  ConvertOption TriangleOsc "onOff" AudioOnOff AudioOnOff where
  convertOption _ _ = identity

type TriangleOscOptional = (onOff :: AudioOnOff)

type TriangleOscAll =
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
  ( ConvertOptionsWithDefaults TriangleOsc { | TriangleOscOptional }
      { | provided }
      { | TriangleOscAll }
  ) =>
  TriangleOscCtor { | provided }
    (CTOR.TriangleOsc /\ {}) where
  triangleOsc provided = CTOR.TriangleOsc all /\ {}
    where
    all :: { | TriangleOscAll }
    all = convertOptionsWithDefaults TriangleOsc defaultTriangleOsc provided
else instance triangleOscCtor2 ::
  Paramable a =>
  TriangleOscCtor a (CTOR.TriangleOsc /\ {}) where
  triangleOsc a =
    CTOR.TriangleOsc { onOff: defaultTriangleOsc.onOff, freq: (paramize a) } /\
      {}

type CTriangleOsc = CTOR.TriangleOsc /\ {}

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
  -> (CTOR.Tumult n terminus inputs) /\ r
tumult = Tuple <<< CTOR.Tumult <<< { tumult: _ }

type CTumult :: forall k1 k2. Type -> k1 -> k2 -> Type -> Type
type CTumult (n :: Type) terminus inputs r =
  (CTOR.Tumult n terminus inputs) /\ r

----------
-- | Apply distorion to audio
-- |
-- | ```purescript
-- | waveShaper (Proxy :: _ "my-wave") OversampleNone { buf: playBuf "my-track" }
-- | ```
waveShaper
  :: forall b c
   . IsOversample b
  => BrowserFloatArray
  -> b
  -> c
  -> CTOR.WaveShaper b /\ c
waveShaper a b c = Tuple (CTOR.WaveShaper { floatArray: a, oversample: b }) c

type CWaveShaper b c = CTOR.WaveShaper b /\ c

---------------
-- | A reference to a node in a graph.
type Ref = Unit /\ {}

-- | A reference to a node in a graph.
ref :: Ref
ref = unit /\ {}
