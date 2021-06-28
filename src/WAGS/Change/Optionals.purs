-- | This module provides functions for the construction of audio units that more closely resemble the overloaded constructors of the Web Audio API.
module WAGS.Change.Optionals where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec as V
import WAGS.Graph.AudioUnit (APOnOff, OnOff(..))
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Paramable (class Paramable, paramize, class OnOffable, onOffIze)
import WAGS.Graph.Parameter (class MM, AudioParameter, mm)

-----------
data Allpass
  = Allpass

instance change_convertAllpassFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption Allpass "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertAllpassQ :: (Paramable b, MM a (Maybe b)) => ConvertOption Allpass "q" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type AllpassOptional
  = ( q :: Maybe AudioParameter, freq :: Maybe AudioParameter )

type AllpassAll
  = ( | AllpassOptional )

defaultAllpass :: { | AllpassOptional }
defaultAllpass = { freq: Nothing, q: Nothing }

class AllpassCtor i allpass | i -> allpass where
  -- | Change an allpass filter
  -- |
  -- | ```purescript
  -- | allpass { freq: 440.0 }
  -- | allpass { freq: 440.0, q: 1.0 }
  -- | allpass 440.0
  -- | ```
  allpass :: i -> allpass

instance change_allpassCtor1 ::
  ( ConvertOptionsWithDefaults Allpass { | AllpassOptional } { | provided } { | AllpassAll }
    ) =>
  AllpassCtor { | provided } (CTOR.Allpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  allpass provided = CTOR.Allpass all.freq all.q
    where
    all :: { | AllpassAll }
    all = convertOptionsWithDefaults Allpass defaultAllpass provided
else instance change_allpassCtor2 :: (Paramable b, MM a (Maybe b)) => AllpassCtor a (CTOR.Allpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  allpass a = CTOR.Allpass (paramize <$> mm a) defaultAllpass.q

type DAllpass
  = CTOR.Allpass (Maybe AudioParameter) (Maybe AudioParameter)

------
data Bandpass
  = Bandpass

instance change_convertBandpassFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption Bandpass "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertBandpassQ :: (Paramable b, MM a (Maybe b)) => ConvertOption Bandpass "q" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type BandpassOptional
  = ( freq :: Maybe AudioParameter, q :: Maybe AudioParameter )

type BandpassAll
  = ( | BandpassOptional )

defaultBandpass :: { | BandpassOptional }
defaultBandpass = { freq: Nothing, q: Nothing }

class BandpassCtor i bandpass | i -> bandpass where
  -- | Create a bandpass filter, connecting it to another unit
  -- |
  -- | ```purescript
  -- | bandpass { freq: 440.0 } { sinOsc: unit }
  -- | bandpass { freq: 440.0, q: 1.0 } { sinOsc: unit }
  -- | bandpass 440.0 { sinOsc: unit }
  -- | ```
  bandpass :: i -> bandpass

instance change_bandpassCtor1 ::
  ( ConvertOptionsWithDefaults Bandpass { | BandpassOptional } { | provided } { | BandpassAll }
    ) =>
  BandpassCtor { | provided } (CTOR.Bandpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  bandpass provided = CTOR.Bandpass all.freq all.q
    where
    all :: { | BandpassAll }
    all = convertOptionsWithDefaults Bandpass defaultBandpass provided
else instance change_bandpassCtor2 :: (Paramable b, MM a (Maybe b)) => BandpassCtor a (CTOR.Bandpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  bandpass a = CTOR.Bandpass (paramize <$> mm a) defaultBandpass.q

type DBandpass
  = CTOR.Bandpass (Maybe AudioParameter) (Maybe AudioParameter)

------
data Constant
  = Constant

instance change_convertConstantOffset :: (Paramable b, MM a (Maybe b)) => ConvertOption Constant "offset" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertConstantAPOnOff :: (OnOffable b, MM a (Maybe b)) => ConvertOption Constant "onOff" a (Maybe APOnOff) where
  convertOption _ _ = map onOffIze <<< mm

type ConstantOptional
  = ( onOff :: APOnOff )

type ConstantAll
  = ( offset :: AudioParameter
    | ConstantOptional
    )

defaultConstant :: { | ConstantOptional }
defaultConstant = { onOff: pure On }

class ConstantCtor i o | i -> o where
  -- | Change a constant value
  -- |
  -- | ```purescript
  -- | constant 0.5
  -- | ```
  constant :: i -> o

instance change_constantCtor1 ::
  ( ConvertOptionsWithDefaults Constant { | ConstantOptional } { | provided } { | ConstantAll }
    ) =>
  ConstantCtor { | provided } (CTOR.Constant APOnOff AudioParameter /\ {}) where
  constant provided = CTOR.Constant all.onOff all.offset /\ {}
    where
    all :: { | ConstantAll }
    all = convertOptionsWithDefaults Constant defaultConstant provided
else instance change_constantCtor2 :: Paramable a => ConstantCtor a (CTOR.Constant APOnOff AudioParameter /\ {}) where
  constant a = CTOR.Constant defaultConstant.onOff (paramize a) /\ {}

type DConstant
  = CTOR.Constant (Maybe APOnOff) (Maybe AudioParameter)

------
-- | Change a delay unit.
-- |
-- | ```purescript
-- | delay 0.5 (playBuf "track")
-- | ```
delay ::
  forall a b.
  Paramable b =>
  MM a (Maybe b) =>
  a -> CTOR.Delay (Maybe AudioParameter)
delay gvsv = CTOR.Delay (paramize <$> (mm gvsv))

type DDelay
  = CTOR.Delay AudioParameter

------
data DynamicsCompressor
  = DynamicsCompressor

instance change_convertDynamicsCompressorThreshold :: (Paramable b, MM a (Maybe b)) => ConvertOption DynamicsCompressor "threshold" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertDynamicsCompressorKnee :: (Paramable b, MM a (Maybe b)) => ConvertOption DynamicsCompressor "knee" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertDynamicsCompressorRatio :: (Paramable b, MM a (Maybe b)) => ConvertOption DynamicsCompressor "ratio" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertDynamicsCompressorAttack :: (Paramable b, MM a (Maybe b)) => ConvertOption DynamicsCompressor "attack" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertDynamicsCompressorRelease :: (Paramable b, MM a (Maybe b)) => ConvertOption DynamicsCompressor "release" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type DynamicsCompressorOptional
  = ( threshold :: Maybe AudioParameter
    , knee :: Maybe AudioParameter
    , ratio :: Maybe AudioParameter
    , attack :: Maybe AudioParameter
    , release :: Maybe AudioParameter
    )

type DynamicsCompressorAll
  = ( | DynamicsCompressorOptional )

defaultDynamicsCompressor :: { | DynamicsCompressorOptional }
defaultDynamicsCompressor =
  { threshold: Nothing
  , knee: Nothing
  , ratio: Nothing
  , attack: Nothing
  , release: Nothing
  }

class DynamicsCompressorCtor i compressor | i -> compressor where
  -- | Change a compressor.
  -- |
  -- | ```purescript
  -- | compressor { threshold: -10.0 } { buf: playBuf "track" }
  -- | compressor { knee: 20.0, ratio: 10.0 } { buf: playBuf "track" }
  -- | compressor { attack: 0.01, release: 0.3 } { buf: playBuf "track" }
  -- | ```
  compressor :: i -> compressor

instance change_compressorCTor ::
  ( ConvertOptionsWithDefaults DynamicsCompressor { | DynamicsCompressorOptional } { | provided } { | DynamicsCompressorAll }
    ) =>
  DynamicsCompressorCtor { | provided } (CTOR.DynamicsCompressor (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter)) where
  compressor provided =
    CTOR.DynamicsCompressor
      all.threshold
      all.knee
      all.ratio
      all.attack
      all.release
    where
    all :: { | DynamicsCompressorAll }
    all = convertOptionsWithDefaults DynamicsCompressor defaultDynamicsCompressor provided

type DDynamicsCompressor
  = CTOR.DynamicsCompressor (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter)

------
gain :: forall a b. Paramable b => MM a (Maybe b) => a -> CTOR.Gain (Maybe AudioParameter)
gain a = CTOR.Gain (paramize <$> mm a)

type Mix
  = CTOR.Gain AudioParameter

type DGain
  = CTOR.Gain AudioParameter

------
data Highpass
  = Highpass

instance change_convertHighpassFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption Highpass "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertHighpassQ :: (Paramable b, MM a (Maybe b)) => ConvertOption Highpass "q" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type HighpassOptional
  = ( freq :: Maybe AudioParameter, q :: Maybe AudioParameter )

type HighpassAll
  = ( | HighpassOptional )

defaultHighpass :: { | HighpassOptional }
defaultHighpass = { freq: Nothing, q: Nothing }

class HighpassCtor i highpass | i -> highpass where
  -- | Change a highpass filter
  -- |
  -- | ```purescript
  -- | highpass { freq: 440.0 }
  -- | highpass { freq: 440.0, q: 1.0 }
  -- | highpass 440.0
  -- | ```
  highpass :: i -> highpass

instance change_highpassCtor1 ::
  ( ConvertOptionsWithDefaults Highpass { | HighpassOptional } { | provided } { | HighpassAll }
    ) =>
  HighpassCtor { | provided } (CTOR.Highpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  highpass provided = CTOR.Highpass all.freq all.q
    where
    all :: { | HighpassAll }
    all = convertOptionsWithDefaults Highpass defaultHighpass provided
else instance change_highpassCtor2 :: (Paramable b, MM a (Maybe b)) => HighpassCtor a (CTOR.Highpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  highpass a = CTOR.Highpass (paramize <$> mm a) defaultHighpass.q

type DHighpass
  = CTOR.Highpass (Maybe AudioParameter) (Maybe AudioParameter)

------
data Highshelf
  = Highshelf

instance change_convertHighshelfFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption Highshelf "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertHighshelfQ :: (Paramable b, MM a (Maybe b)) => ConvertOption Highshelf "gain" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type HighshelfOptional
  = ( freq :: Maybe AudioParameter, gain :: Maybe AudioParameter )

type HighshelfAll
  = ( | HighshelfOptional )

defaultHighshelf :: { | HighshelfOptional }
defaultHighshelf = { freq: Nothing, gain: Nothing }

class HighshelfCtor i highshelf | i -> highshelf where
  -- | Change a highshelf filter
  -- |
  -- | ```purescript
  -- | highshelf { freq: 440.0 }
  -- | highshelf { freq: 440.0, gain: 1.0 }
  -- | highshelf 440.0
  -- | ```
  highshelf :: i -> highshelf

instance change_highshelfCtor1 ::
  ( ConvertOptionsWithDefaults Highshelf { | HighshelfOptional } { | provided } { | HighshelfAll }
    ) =>
  HighshelfCtor { | provided } (CTOR.Highshelf (Maybe AudioParameter) (Maybe AudioParameter)) where
  highshelf provided = CTOR.Highshelf all.freq all.gain
    where
    all :: { | HighshelfAll }
    all = convertOptionsWithDefaults Highshelf defaultHighshelf provided
else instance change_highshelfCtor2 :: (Paramable b, MM a (Maybe b)) => HighshelfCtor a (CTOR.Highshelf (Maybe AudioParameter) (Maybe AudioParameter)) where
  highshelf a = CTOR.Highshelf (paramize <$> mm a) defaultHighshelf.gain

type DHighshelf
  = CTOR.Highshelf (Maybe AudioParameter) (Maybe AudioParameter)

----
data LoopBuf
  = LoopBuf

instance change_convertLoopBufBuffer :: (MM a (Maybe String)) => ConvertOption LoopBuf "buffer" a (Maybe String) where
  convertOption _ _ = mm

instance change_convertLoopBufPlaybackRate :: (Paramable b, MM a (Maybe b)) => ConvertOption LoopBuf "playbackRate" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertLoopBufAPOnOff :: (OnOffable b, MM a (Maybe b)) => ConvertOption LoopBuf "onOff" a (Maybe APOnOff) where
  convertOption _ _ = map onOffIze <<< mm

instance change_convertLoopBufStart :: (MM a (Maybe Number)) => ConvertOption LoopBuf "loopStart" a (Maybe Number) where
  convertOption _ _ = mm

instance change_convertLoopBufEnd :: (MM a (Maybe Number)) => ConvertOption LoopBuf "loopEnd" a (Maybe Number) where
  convertOption _ _ = mm

type LoopBufOptional
  = ( buffer :: Maybe String
    , playbackRate :: Maybe AudioParameter
    , onOff :: Maybe APOnOff
    , loopStart :: Maybe Number
    , loopEnd :: Maybe Number
    )

type LoopBufAll
  = ( | LoopBufOptional )

defaultLoopBuf :: { | LoopBufOptional }
defaultLoopBuf = { buffer: Nothing, playbackRate: Nothing, onOff: Nothing, loopStart: Nothing, loopEnd: Nothing }

class LoopBufCtor i loopBuf | i -> loopBuf where
  -- | Change a looping buffer.
  -- |
  -- | ```purescript
  -- | loopBuf { playbackRate: 1.0 } "track"
  -- | loopBuf { playbackRate: 1.0, loopStart: 0.5 } "track"
  -- | loopBuf "track"
  -- | ```
  loopBuf :: i -> loopBuf

instance change_loopBufCtor1 ::
  ( ConvertOptionsWithDefaults LoopBuf { | LoopBufOptional } { | provided } { | LoopBufAll }
    ) =>
  LoopBufCtor { | provided } (CTOR.LoopBuf (Maybe String) (Maybe APOnOff) (Maybe AudioParameter) (Maybe Number) (Maybe Number)) where
  loopBuf provided = CTOR.LoopBuf all.buffer all.onOff all.playbackRate all.loopStart all.loopEnd
    where
    all :: { | LoopBufAll }
    all = convertOptionsWithDefaults LoopBuf defaultLoopBuf provided
else instance change_loopBufCtor2 :: (Paramable b, MM a (Maybe b)) => LoopBufCtor a (CTOR.LoopBuf (Maybe String) (Maybe APOnOff) (Maybe AudioParameter) (Maybe Number) (Maybe Number)) where
  loopBuf rate =
    CTOR.LoopBuf
      defaultLoopBuf.buffer
      defaultLoopBuf.onOff
      (paramize <$> mm rate)
      defaultLoopBuf.loopStart
      defaultLoopBuf.loopEnd

type DLoopBuf
  = CTOR.LoopBuf String (Maybe APOnOff) (Maybe AudioParameter) Number Number

-----
data Lowpass
  = Lowpass

instance change_convertLowpassFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption Lowpass "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertLowpassQ :: (Paramable b, MM a (Maybe b)) => ConvertOption Lowpass "q" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type LowpassOptional
  = ( q :: Maybe AudioParameter, freq :: Maybe AudioParameter )

type LowpassAll
  = ( | LowpassOptional )

defaultLowpass :: { | LowpassOptional }
defaultLowpass = { freq: Nothing, q: Nothing }

class LowpassCtor i lowpass | i -> lowpass where
  -- | Change a lowpass filter
  -- |
  -- | ```purescript
  -- | lowpass { freq: 440.0 }
  -- | lowpass { freq: 440.0, q: 1.0 }
  -- | lowpass 440.0
  -- | ```
  lowpass :: i -> lowpass

instance change_lowpassCtor1 ::
  ( ConvertOptionsWithDefaults Lowpass { | LowpassOptional } { | provided } { | LowpassAll }
    ) =>
  LowpassCtor { | provided } (CTOR.Lowpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  lowpass provided = CTOR.Lowpass all.freq all.q
    where
    all :: { | LowpassAll }
    all = convertOptionsWithDefaults Lowpass defaultLowpass provided
else instance change_lowpassCtor2 :: (Paramable b, MM a (Maybe b)) => LowpassCtor a (CTOR.Lowpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  lowpass a = CTOR.Lowpass (paramize <$> mm a) defaultLowpass.q

type DLowpass
  = CTOR.Lowpass (Maybe AudioParameter) (Maybe AudioParameter)

-----
data Lowshelf
  = Lowshelf

instance change_convertLowshelfFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption Lowshelf "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertLowshelfQ :: (Paramable b, MM a (Maybe b)) => ConvertOption Lowshelf "gain" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type LowshelfOptional
  = ( freq :: Maybe AudioParameter, gain :: Maybe AudioParameter )

type LowshelfAll
  = ( | LowshelfOptional )

defaultLowshelf :: { | LowshelfOptional }
defaultLowshelf = { freq: Nothing, gain: Nothing }

class LowshelfCtor i lowshelf | i -> lowshelf where
  -- | Change a lowshelf filter
  -- |
  -- | ```purescript
  -- | lowshelf { freq: 440.0 }
  -- | lowshelf { freq: 440.0, gain: 1.0 }
  -- | lowshelf 440.0
  -- | ```
  lowshelf :: i -> lowshelf

instance change_lowshelfCtor1 ::
  ( ConvertOptionsWithDefaults Lowshelf { | LowshelfOptional } { | provided } { | LowshelfAll }
    ) =>
  LowshelfCtor { | provided } (CTOR.Lowshelf (Maybe AudioParameter) (Maybe AudioParameter)) where
  lowshelf provided = CTOR.Lowshelf all.freq all.gain
    where
    all :: { | LowshelfAll }
    all = convertOptionsWithDefaults Lowshelf defaultLowshelf provided
else instance change_lowshelfCtor2 :: (Paramable b, MM a (Maybe b)) => LowshelfCtor a (CTOR.Lowshelf (Maybe AudioParameter) (Maybe AudioParameter)) where
  lowshelf a = CTOR.Lowshelf (paramize <$> mm a) defaultLowshelf.gain

type DLowshelf
  = CTOR.Lowshelf (Maybe AudioParameter) (Maybe AudioParameter)

--------
data Notch
  = Notch

instance change_convertNotchFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption Notch "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertNotchQ :: (Paramable b, MM a (Maybe b)) => ConvertOption Notch "q" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type NotchOptional
  = ( q :: Maybe AudioParameter, freq :: Maybe AudioParameter )

type NotchAll
  = ( | NotchOptional )

defaultNotch :: { | NotchOptional }
defaultNotch = { freq: Nothing, q: Nothing }

class NotchCtor i notch | i -> notch where
  -- | Change a notch (band-reject) filter
  -- |
  -- | ```purescript
  -- | notch { freq: 440.0 }
  -- | notch { freq: 440.0, gain: 1.0 }
  -- | notch 440.0
  -- | ```
  notch :: i -> notch

instance change_notchCtor1 ::
  ( ConvertOptionsWithDefaults Notch { | NotchOptional } { | provided } { | NotchAll }
    ) =>
  NotchCtor { | provided } (CTOR.Notch (Maybe AudioParameter) (Maybe AudioParameter)) where
  notch provided = CTOR.Notch all.freq all.q
    where
    all :: { | NotchAll }
    all = convertOptionsWithDefaults Notch defaultNotch provided
else instance change_notchCtor2 :: (Paramable b, MM a (Maybe b)) => NotchCtor a (CTOR.Notch (Maybe AudioParameter) (Maybe AudioParameter)) where
  notch a = CTOR.Notch (paramize <$> mm a) defaultNotch.q

type DNotch
  = CTOR.Notch (Maybe AudioParameter) (Maybe AudioParameter)

----------------
data Peaking
  = Peaking

instance change_convertPeakingFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption Peaking "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertPeakingQ :: (Paramable b, MM a (Maybe b)) => ConvertOption Peaking "q" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertPeakingGain :: (Paramable b, MM a (Maybe b)) => ConvertOption Peaking "gain" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

type PeakingOptional
  = ( q :: Maybe AudioParameter, gain :: Maybe AudioParameter, freq :: Maybe AudioParameter )

type PeakingAll
  = ( | PeakingOptional )

defaultPeaking :: { | PeakingOptional }
defaultPeaking = { freq: Nothing, q: Nothing, gain: Nothing }

class PeakingCtor i peaking | i -> peaking where
  -- | Change a peaking filter
  -- |
  -- | ```purescript
  -- | peaking { freq: 440.0 }
  -- | peaking { freq: 440.0, gain: 1.0 }
  -- | peaking 440.0
  -- | ```
  peaking :: i -> peaking

instance change_peakingCtor1 ::
  ( ConvertOptionsWithDefaults Peaking { | PeakingOptional } { | provided } { | PeakingAll }
    ) =>
  PeakingCtor { | provided } (CTOR.Peaking (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter)) where
  peaking provided = (CTOR.Peaking all.freq all.q all.gain)
    where
    all :: { | PeakingAll }
    all = convertOptionsWithDefaults Peaking defaultPeaking provided
else instance change_peakingCtor2 :: (Paramable b, MM a (Maybe b)) => PeakingCtor a (CTOR.Peaking (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter)) where
  peaking a = (CTOR.Peaking (paramize <$> mm a) defaultPeaking.q defaultPeaking.gain)

type DPeaking
  = CTOR.Peaking (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter)

------
------
class CanBeCoercedToPeriodicOsc (canBeCoercedToPeriodicOsc :: Type)

instance change_canBeCoercedToPeriodicOscString :: CanBeCoercedToPeriodicOsc String

instance change_canBeCoercedToPeriodicOscV :: CanBeCoercedToPeriodicOsc (V.Vec size Number /\ V.Vec size Number)

data PeriodicOsc
  = PeriodicOsc

instance change_convertPeriodicOscFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption PeriodicOsc "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertPeriodicOscAPOnOff :: (OnOffable b, MM a (Maybe b)) => ConvertOption PeriodicOsc "onOff" a (Maybe APOnOff) where
  convertOption _ _ = map onOffIze <<< mm

instance change_convertPeriodicOscWave :: (MM mWave (Maybe wave), CanBeCoercedToPeriodicOsc wave) => ConvertOption PeriodicOsc "waveform" mWave (Maybe wave) where
  convertOption _ _ = mm

type PeriodicOscOptional wave
  = ( onOff :: Maybe APOnOff
    , freq :: Maybe AudioParameter
    , wave :: Maybe wave
    )

type PeriodicOscAll wave
  = ( | PeriodicOscOptional wave )

defaultPeriodicOsc :: forall wave. { | PeriodicOscOptional wave }
defaultPeriodicOsc = { onOff: Nothing, freq: Nothing, wave: Nothing }

class PeriodicOscCtor i o | i -> o where
  -- | Change a periodicOsc value
  -- |
  -- | ```purescript
  -- | periodicOsc "my-osc" 0.5
  -- | ```
  periodicOsc :: i -> o

instance change_periodicOscCtor1 ::
  ( ConvertOptionsWithDefaults PeriodicOsc { | PeriodicOscOptional wave } { | provided } { | PeriodicOscAll wave }
    ) =>
  PeriodicOscCtor { | provided } (CTOR.PeriodicOsc (Maybe wave) (Maybe APOnOff) (Maybe AudioParameter)) where
  periodicOsc provided = CTOR.PeriodicOsc all.wave all.onOff all.freq
    where
    all :: { | PeriodicOscAll wave }
    all = convertOptionsWithDefaults PeriodicOsc defaultPeriodicOsc provided
else instance change_periodicOscCtor2 :: (MM mWave (Maybe wave), CanBeCoercedToPeriodicOsc wave, Paramable b, MM a (Maybe b)) => PeriodicOscCtor mWave (a -> CTOR.PeriodicOsc (Maybe wave) (Maybe APOnOff) (Maybe AudioParameter)) where
  periodicOsc wave a = CTOR.PeriodicOsc (mm wave) defaultPeriodicOsc.onOff (paramize <$> mm a)

type DPeriodicOsc periodicOsc
  = CTOR.PeriodicOsc periodicOsc APOnOff AudioParameter

---
data PlayBuf
  = PlayBuf

instance change_convertPlayBufBuffer :: (MM a (Maybe String)) => ConvertOption PlayBuf "buffer" a (Maybe String) where
  convertOption _ _ = mm

instance change_convertPlayBufPlaybackRate :: (Paramable b, MM a (Maybe b)) => ConvertOption PlayBuf "playbackRate" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertPlayBufAPOnOff :: (MM mAPOnOff (Maybe APOnOff)) => ConvertOption PlayBuf "onOff" mAPOnOff (Maybe APOnOff) where
  convertOption _ _ = mm

instance change_convertPlayBufBufferOffset :: (MM mOffset (Maybe Number)) => ConvertOption PlayBuf "bufferOffset" mOffset (Maybe Number) where
  convertOption _ _ = mm

type PlayBufOptional
  = ( buffer :: Maybe String, playbackRate :: Maybe AudioParameter, onOff :: Maybe APOnOff, bufferOffset :: Maybe Number )

type PlayBufAll
  = ( | PlayBufOptional )

defaultPlayBuf :: { | PlayBufOptional }
defaultPlayBuf = { buffer: Nothing, playbackRate: Nothing, onOff: Nothing, bufferOffset: Nothing }

class PlayBufCtor i playBuf | i -> playBuf where
  -- | Change a unit that plays from a buffer.
  -- |
  -- | ```purescript
  -- | playBuf { playbackRate: 1.0 } "track"
  -- | playBuf { playbackRate: 1.0, bufferOffset: 0.5 } "track"
  -- | playBuf "track"
  -- | ```
  playBuf :: i -> playBuf

instance change_playBufCtor1 ::
  ConvertOptionsWithDefaults PlayBuf { | PlayBufOptional } { | provided } { | PlayBufAll } =>
  PlayBufCtor { | provided } (CTOR.PlayBuf (Maybe String) (Maybe Number) (Maybe APOnOff) (Maybe AudioParameter)) where
  playBuf provided = CTOR.PlayBuf all.buffer all.bufferOffset all.onOff all.playbackRate
    where
    all :: { | PlayBufAll }
    all = convertOptionsWithDefaults PlayBuf defaultPlayBuf provided
else instance change_playBufCtor2 :: (Paramable b, MM a (Maybe b)) => PlayBufCtor a (CTOR.PlayBuf (Maybe String) (Maybe Number) (Maybe APOnOff) (Maybe AudioParameter)) where
  playBuf rate =
    CTOR.PlayBuf
      defaultPlayBuf.buffer
      defaultPlayBuf.bufferOffset
      defaultPlayBuf.onOff
      (paramize <$> mm rate)

type DPlayBuf
  = CTOR.PlayBuf String Number (Maybe APOnOff) (Maybe AudioParameter)

------
class SawtoothOscCtor i o | i -> o where
  -- | Change a sawtooth oscillator
  -- |
  -- | ```purescript
  -- | sawtoothOsc 440.0
  -- | sawtoothOsc On 440.0
  -- | ```
  sawtoothOsc :: i -> o

instance change_sawtoothOsc2 ::
  (MM mAPOnOff (Maybe APOnOff), Paramable b, MM a (Maybe b)) =>
  SawtoothOscCtor mAPOnOff (a -> CTOR.SawtoothOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  sawtoothOsc oo gvsv = CTOR.SawtoothOsc (mm oo) (paramize <$> (mm gvsv))
else instance change_sawtoothOsc1 ::
  (Paramable b, MM a (Maybe b)) =>
  SawtoothOscCtor a (CTOR.SawtoothOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  sawtoothOsc gvsv = CTOR.SawtoothOsc Nothing (paramize <$> (mm gvsv))

type DSawtoothOsc
  = CTOR.SawtoothOsc (Maybe APOnOff) (Maybe AudioParameter)

------
data SinOsc
  = SinOsc

instance change_convertSinOscFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption SinOsc "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertSinOscAPOnOff :: (OnOffable b, MM a (Maybe b)) => ConvertOption SinOsc "onOff" a (Maybe APOnOff) where
  convertOption _ _ = map onOffIze <<< mm

type SinOscOptional
  = ( onOff :: APOnOff )

type SinOscAll
  = ( freq :: AudioParameter
    | SinOscOptional
    )

defaultSinOsc :: { | SinOscOptional }
defaultSinOsc = { onOff: pure On }

class SinOscCtor i o | i -> o where
  -- | Change a sinOsc value
  -- |
  -- | ```purescript
  -- | sinOsc 0.5
  -- | ```
  sinOsc :: i -> o

instance change_sinOscCtor1 ::
  ( ConvertOptionsWithDefaults SinOsc { | SinOscOptional } { | provided } { | SinOscAll }
    ) =>
  SinOscCtor { | provided } (CTOR.SinOsc APOnOff AudioParameter /\ {}) where
  sinOsc provided = CTOR.SinOsc all.onOff all.freq /\ {}
    where
    all :: { | SinOscAll }
    all = convertOptionsWithDefaults SinOsc defaultSinOsc provided
else instance change_sinOscCtor2 :: Paramable a => SinOscCtor a (CTOR.SinOsc APOnOff AudioParameter /\ {}) where
  sinOsc a = CTOR.SinOsc defaultSinOsc.onOff (paramize a) /\ {}

type DSinOsc
  = CTOR.SinOsc (Maybe APOnOff) (Maybe AudioParameter)

------
data SquareOsc
  = SquareOsc

instance change_convertSquareOscFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption SquareOsc "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertSquareOscAPOnOff :: (OnOffable b, MM a (Maybe b)) => ConvertOption SquareOsc "onOff" a (Maybe APOnOff) where
  convertOption _ _ = map onOffIze <<< mm

type SquareOscOptional
  = ( onOff :: APOnOff )

type SquareOscAll
  = ( freq :: AudioParameter
    | SquareOscOptional
    )

defaultSquareOsc :: { | SquareOscOptional }
defaultSquareOsc = { onOff: pure On }

class SquareOscCtor i o | i -> o where
  -- | Change a squareOsc value
  -- |
  -- | ```purescript
  -- | squareOsc 0.5
  -- | ```
  squareOsc :: i -> o

instance change_squareOscCtor1 ::
  ( ConvertOptionsWithDefaults SquareOsc { | SquareOscOptional } { | provided } { | SquareOscAll }
    ) =>
  SquareOscCtor { | provided } (CTOR.SquareOsc APOnOff AudioParameter /\ {}) where
  squareOsc provided = CTOR.SquareOsc all.onOff all.freq /\ {}
    where
    all :: { | SquareOscAll }
    all = convertOptionsWithDefaults SquareOsc defaultSquareOsc provided
else instance change_squareOscCtor2 :: Paramable a => SquareOscCtor a (CTOR.SquareOsc APOnOff AudioParameter /\ {}) where
  squareOsc a = CTOR.SquareOsc defaultSquareOsc.onOff (paramize a) /\ {}

type DSquareOsc
  = CTOR.SquareOsc (Maybe APOnOff) (Maybe AudioParameter)

------
-- | Pan audio.
-- |
-- | ```purescript
-- | pan 0.5 { buf: playBuf "my-track" }
-- | ```
pan ::
  forall a b.
  Paramable b =>
  MM a (Maybe b) =>
  a -> CTOR.StereoPanner (Maybe AudioParameter)
pan gvsv = CTOR.StereoPanner (paramize <$> (mm gvsv))

type DStereoPanner
  = CTOR.StereoPanner (Maybe AudioParameter)

------
data TriangleOsc
  = TriangleOsc

instance change_convertTriangleOscFrequency :: (Paramable b, MM a (Maybe b)) => ConvertOption TriangleOsc "freq" a (Maybe AudioParameter) where
  convertOption _ _ = map paramize <<< mm

instance change_convertTriangleOscAPOnOff :: (OnOffable b, MM a (Maybe b)) => ConvertOption TriangleOsc "onOff" a (Maybe APOnOff) where
  convertOption _ _ = map onOffIze <<< mm

type TriangleOscOptional
  = ( onOff :: APOnOff )

type TriangleOscAll
  = ( freq :: AudioParameter
    | TriangleOscOptional
    )

defaultTriangleOsc :: { | TriangleOscOptional }
defaultTriangleOsc = { onOff: pure On }

class TriangleOscCtor i o | i -> o where
  -- | Change a triangleOsc value
  -- |
  -- | ```purescript
  -- | triangleOsc 0.5
  -- | ```
  triangleOsc :: i -> o

instance change_triangleOscCtor1 ::
  ( ConvertOptionsWithDefaults TriangleOsc { | TriangleOscOptional } { | provided } { | TriangleOscAll }
    ) =>
  TriangleOscCtor { | provided } (CTOR.TriangleOsc APOnOff AudioParameter /\ {}) where
  triangleOsc provided = CTOR.TriangleOsc all.onOff all.freq /\ {}
    where
    all :: { | TriangleOscAll }
    all = convertOptionsWithDefaults TriangleOsc defaultTriangleOsc provided
else instance change_triangleOscCtor2 :: Paramable a => TriangleOscCtor a (CTOR.TriangleOsc APOnOff AudioParameter /\ {}) where
  triangleOsc a = CTOR.TriangleOsc defaultTriangleOsc.onOff (paramize a) /\ {}

type DTriangleOsc
  = CTOR.TriangleOsc (Maybe APOnOff) (Maybe AudioParameter)
