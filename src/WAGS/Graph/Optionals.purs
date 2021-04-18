-- | This module provides functions for the construction of audio units that more closely resemble the overloaded constructors of the Web Audio API.
module WAGS.Graph.Optionals where

import Prelude
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy)
import WAGS.Change (class SetterVal, setterVal)
import WAGS.Create (class InitialVal, initialVal)
import WAGS.Graph.Constructors (OnOff(..))
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Decorators (class IsAudio, class IsAudioOrF, class IsMultiAudio, class IsMultiAudioOrF, class IsOversample)
import WAGS.Graph.Parameter (AudioParameter, param)

type GetSetAP
  = Tuple AudioParameter (AudioParameter -> AudioParameter)

defaultGetSetAP :: Number -> GetSetAP
defaultGetSetAP n = Tuple p (const p)
  where
  p = param n

-----------
data Allpass'
  = Allpass'

instance convertAllpassFrequency :: (InitialVal a, SetterVal a) => ConvertOption Allpass' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertAllpassQ :: (InitialVal a, SetterVal a) => ConvertOption Allpass' "q" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type AllpassOptional
  = ( q :: GetSetAP )

type AllpassAll
  = ( freq :: GetSetAP
    | AllpassOptional
    )

defaultAllpass :: { | AllpassOptional }
defaultAllpass = { q: defaultGetSetAP 1.0 }

class AllpassCtor i allpass | i -> allpass where
  -- | Make an allpass filter
  -- |
  -- | ```purescript
  -- | allpass { freq: 440.0 } sinOsc 440.0
  -- | allpass { freq: 440.0, q: 1.0 } sinOsc 440.0
  -- | allpass 440.0 sinOsc 440.0
  -- | ```
  allpass :: i -> allpass

instance allpassCtor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults Allpass' { | AllpassOptional } { | provided } { | AllpassAll }
  ) =>
  AllpassCtor { | provided } (c -> CTOR.Allpass GetSetAP GetSetAP c) where
  allpass provided cont = CTOR.Allpass all.freq all.q cont
    where
    all :: { | AllpassAll }
    all = convertOptionsWithDefaults Allpass' defaultAllpass provided
else instance allpassCtor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => AllpassCtor a (b -> (CTOR.Allpass GetSetAP GetSetAP b)) where
  allpass a cont = CTOR.Allpass (Tuple (initialVal a) (setterVal a)) defaultAllpass.q cont

------
data Bandpass'
  = Bandpass'

instance convertBandpassFrequency :: (InitialVal a, SetterVal a) => ConvertOption Bandpass' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertBandpassQ :: (InitialVal a, SetterVal a) => ConvertOption Bandpass' "q" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type BandpassOptional
  = ( q :: GetSetAP )

type BandpassAll
  = ( freq :: GetSetAP
    | BandpassOptional
    )

defaultBandpass :: { | BandpassOptional }
defaultBandpass = { q: defaultGetSetAP 1.0 }

class BandpassCtor i bandpass | i -> bandpass where
  -- | Make a bandpass filter
  -- |
  -- | ```purescript
  -- | bandpass { freq: 440.0 } sinOsc 440.0
  -- | bandpass { freq: 440.0, q: 1.0 } sinOsc 440.0
  -- | bandpass 440.0 sinOsc 440.0
  -- | ```
  bandpass :: i -> bandpass

instance bandpassCtor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults Bandpass' { | BandpassOptional } { | provided } { | BandpassAll }
  ) =>
  BandpassCtor { | provided } (c -> CTOR.Bandpass GetSetAP GetSetAP c) where
  bandpass provided cont = CTOR.Bandpass all.freq all.q cont
    where
    all :: { | BandpassAll }
    all = convertOptionsWithDefaults Bandpass' defaultBandpass provided
else instance bandpassCtor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => BandpassCtor a (b -> (CTOR.Bandpass GetSetAP GetSetAP b)) where
  bandpass a cont = CTOR.Bandpass (Tuple (initialVal a) (setterVal a)) defaultBandpass.q cont

------
class ConstantCtor i o | i -> o where
  -- | Make a constant value
  -- |
  -- | ```purescript
  -- | constant 0.5
  -- | constant On 0.5
  -- | ```
  constant :: i -> o

instance constantCtor2 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  ConstantCtor OnOff (a -> CTOR.Constant GetSetAP) where
  constant oo gvsv = CTOR.Constant oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance constantCtor1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  ConstantCtor a (CTOR.Constant GetSetAP) where
  constant gvsv = CTOR.Constant On (Tuple (initialVal gvsv) (setterVal gvsv))

------
-- | Make a convolver, aka reverb.
-- |
-- | ```purescript
-- | convolver (Proxy :: _ "room") (playBuf (Proxy :: _ "track"))
-- | ```
convolver ::
  forall s c.
  IsSymbol s =>
  IsAudioOrF c =>
  Proxy s -> c -> CTOR.Convolver s c
convolver px cont = CTOR.Convolver px cont

------
-- | Make a delay unit.
-- |
-- | ```purescript
-- | delay 0.5 (playBuf (Proxy :: _ "track"))
-- | ```
delay ::
  forall a c.
  InitialVal a =>
  SetterVal a =>
  IsAudioOrF c =>
  a -> c -> CTOR.Delay GetSetAP c
delay gvsv cont = CTOR.Delay (Tuple (initialVal gvsv) (setterVal gvsv)) cont

------
data DynamicsCompressor'
  = DynamicsCompressor'

instance convertDynamicsCompressorThreshold :: (InitialVal a, SetterVal a) => ConvertOption DynamicsCompressor' "threshold" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertDynamicsCompressorKnee :: (InitialVal a, SetterVal a) => ConvertOption DynamicsCompressor' "knee" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertDynamicsCompressorRatio :: (InitialVal a, SetterVal a) => ConvertOption DynamicsCompressor' "ratio" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertDynamicsCompressorAttack :: (InitialVal a, SetterVal a) => ConvertOption DynamicsCompressor' "attack" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertDynamicsCompressorRelease :: (InitialVal a, SetterVal a) => ConvertOption DynamicsCompressor' "release" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type DynamicsCompressorOptional
  = ( threshold :: GetSetAP
    , knee :: GetSetAP
    , ratio :: GetSetAP
    , attack :: GetSetAP
    , release :: GetSetAP
    )

type DynamicsCompressorAll
  = (
    | DynamicsCompressorOptional
    )

defaultDynamicsCompressor :: { | DynamicsCompressorOptional }
defaultDynamicsCompressor =
  { threshold: defaultGetSetAP (-24.0)
  , knee: defaultGetSetAP 30.0
  , ratio: defaultGetSetAP 12.0
  , attack: defaultGetSetAP 0.003
  , release: defaultGetSetAP 0.25
  }

class DynamicsCompressorCtor i compressor | i -> compressor where
  -- | Make a compressor.
  -- |
  -- | ```purescript
  -- | compressor (playBuf (Proxy :: _ "track"))
  -- | compressor { threshold: -10.0 } (playBuf (Proxy :: _ "track"))
  -- | compressor { knee: 20.0, ratio: 10.0 } (playBuf (Proxy :: _ "track"))
  -- | compressor { attack: 0.01, release: 0.3 } (playBuf (Proxy :: _ "track"))
  -- | ```
  compressor :: i -> compressor

instance compressorCTor ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults DynamicsCompressor' { | DynamicsCompressorOptional } { | provided } { | DynamicsCompressorAll }
  ) =>
  DynamicsCompressorCtor { | provided } (c -> CTOR.DynamicsCompressor GetSetAP GetSetAP GetSetAP GetSetAP GetSetAP c) where
  compressor provided cont =
    CTOR.DynamicsCompressor
      all.threshold
      all.knee
      all.ratio
      all.attack
      all.release
      cont
    where
    all :: { | DynamicsCompressorAll }
    all = convertOptionsWithDefaults DynamicsCompressor' defaultDynamicsCompressor provided
else instance compressorCTorNp :: (IsAudioOrF b) => DynamicsCompressorCtor b (CTOR.DynamicsCompressor GetSetAP GetSetAP GetSetAP GetSetAP GetSetAP b) where
  compressor cont =
    CTOR.DynamicsCompressor
      defaultDynamicsCompressor.threshold
      defaultDynamicsCompressor.knee
      defaultDynamicsCompressor.ratio
      defaultDynamicsCompressor.attack
      defaultDynamicsCompressor.release
      cont

------
class GainCtor i gain | i -> gain where
  -- | Make a gain unit
  -- |
  -- | ```purescript
  -- | gain 0.5 (playBuf (Proxy :: _ "hello") /\ playBuf (Proxy :: _ "world") /\ unit)
  -- | ```
  gain :: i -> gain

instance gainCtor1 :: (InitialVal a, SetterVal a, IsMultiAudioOrF b) => GainCtor a (b -> (CTOR.Gain GetSetAP b)) where
  gain a cont = CTOR.Gain (Tuple (initialVal a) (setterVal a)) cont

-- | Mix together several audio units
-- |
-- | ```purescript
-- | mix (playBuf (Proxy :: _ "hello") /\ playBuf (Proxy :: _ "world") /\ unit)
-- | ```
mix :: forall a. IsMultiAudioOrF a => a -> CTOR.Gain GetSetAP a
mix cont = CTOR.Gain (defaultGetSetAP 1.0) cont

------
data Highpass'
  = Highpass'

instance convertHighpassFrequency :: (InitialVal a, SetterVal a) => ConvertOption Highpass' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertHighpassQ :: (InitialVal a, SetterVal a) => ConvertOption Highpass' "q" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type HighpassOptional
  = ( q :: GetSetAP )

type HighpassAll
  = ( freq :: GetSetAP
    | HighpassOptional
    )

defaultHighpass :: { | HighpassOptional }
defaultHighpass = { q: defaultGetSetAP 1.0 }

class HighpassCtor i highpass | i -> highpass where
  -- | Make a highpass filter
  -- |
  -- | ```purescript
  -- | highpass { freq: 440.0 } sinOsc 440.0
  -- | highpass { freq: 440.0, q: 1.0 } sinOsc 440.0
  -- | highpass 440.0 sinOsc 440.0
  -- | ```
  highpass :: i -> highpass

instance highpassCtor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults Highpass' { | HighpassOptional } { | provided } { | HighpassAll }
  ) =>
  HighpassCtor { | provided } (c -> CTOR.Highpass GetSetAP GetSetAP c) where
  highpass provided cont = CTOR.Highpass all.freq all.q cont
    where
    all :: { | HighpassAll }
    all = convertOptionsWithDefaults Highpass' defaultHighpass provided
else instance highpassCtor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => HighpassCtor a (b -> (CTOR.Highpass GetSetAP GetSetAP b)) where
  highpass a cont = CTOR.Highpass (Tuple (initialVal a) (setterVal a)) defaultHighpass.q cont

------
data Highshelf'
  = Highshelf'

instance convertHighshelfFrequency :: (InitialVal a, SetterVal a) => ConvertOption Highshelf' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertHighshelfQ :: (InitialVal a, SetterVal a) => ConvertOption Highshelf' "gain" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type HighshelfOptional
  = ( gain :: GetSetAP )

type HighshelfAll
  = ( freq :: GetSetAP
    | HighshelfOptional
    )

defaultHighshelf :: { | HighshelfOptional }
defaultHighshelf = { gain: defaultGetSetAP 0.0 }

class HighshelfCtor i highshelf | i -> highshelf where
  -- | Make a highshelf filter
  -- |
  -- | ```purescript
  -- | highshelf { freq: 440.0 } sinOsc 440.0
  -- | highshelf { freq: 440.0, gain: 1.0 } sinOsc 440.0
  -- | highshelf 440.0 sinOsc 440.0
  -- | ```
  highshelf :: i -> highshelf

instance highshelfCtor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults Highshelf' { | HighshelfOptional } { | provided } { | HighshelfAll }
  ) =>
  HighshelfCtor { | provided } (c -> CTOR.Highshelf GetSetAP GetSetAP c) where
  highshelf provided cont = CTOR.Highshelf all.freq all.gain cont
    where
    all :: { | HighshelfAll }
    all = convertOptionsWithDefaults Highshelf' defaultHighshelf provided
else instance highshelfCtor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => HighshelfCtor a (b -> (CTOR.Highshelf GetSetAP GetSetAP b)) where
  highshelf a cont = CTOR.Highshelf (Tuple (initialVal a) (setterVal a)) defaultHighshelf.gain cont

----
data LoopBuf'
  = LoopBuf'

instance convertLoopBufPlaybackRate :: (InitialVal a, SetterVal a) => ConvertOption LoopBuf' "playbackRate" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertLoopBufOnOff :: ConvertOption LoopBuf' "onOff" OnOff OnOff where
  convertOption _ _ = identity

instance convertLoopBufStart :: ConvertOption LoopBuf' "start" Number Number where
  convertOption _ _ = identity

instance convertLoopBufEnd :: ConvertOption LoopBuf' "end" Number Number where
  convertOption _ _ = identity

type LoopBufOptional
  = ( playbackRate :: GetSetAP, onOff :: OnOff, start :: Number, end :: Number )

type LoopBufAll
  = ( | LoopBufOptional )

defaultLoopBuf :: { | LoopBufOptional }
defaultLoopBuf = { playbackRate: defaultGetSetAP 1.0, onOff: On, start: 0.0, end: 0.0 }

class LoopBufCtor i loopBuf | i -> loopBuf where
  -- | Make a looping buffer.
  -- |
  -- | ```purescript
  -- | loopBuf { playbackRate: 1.0 } (Proxy :: _ "track")
  -- | loopBuf { playbackRate: 1.0, start: 0.5 } (Proxy :: _ "track")
  -- | loopBuf (Proxy :: _ "track")
  -- | ```
  loopBuf :: i -> loopBuf

instance loopBufCtor1 ::
  ( IsSymbol l
  , ConvertOptionsWithDefaults LoopBuf' { | LoopBufOptional } { | provided } { | LoopBufAll }
  ) =>
  LoopBufCtor { | provided } (Proxy l -> CTOR.LoopBuf l GetSetAP) where
  loopBuf provided proxy = CTOR.LoopBuf proxy all.onOff all.playbackRate all.start all.end
    where
    all :: { | LoopBufAll }
    all = convertOptionsWithDefaults LoopBuf' defaultLoopBuf provided
else instance loopBufCtor2 :: IsSymbol l => LoopBufCtor (Proxy l) (CTOR.LoopBuf l GetSetAP) where
  loopBuf proxy =
    CTOR.LoopBuf
      proxy
      defaultLoopBuf.onOff
      defaultLoopBuf.playbackRate
      defaultLoopBuf.start
      defaultLoopBuf.end

-----
data Lowpass'
  = Lowpass'

instance convertLowpassFrequency :: (InitialVal a, SetterVal a) => ConvertOption Lowpass' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertLowpassQ :: (InitialVal a, SetterVal a) => ConvertOption Lowpass' "q" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type LowpassOptional
  = ( q :: GetSetAP )

type LowpassAll
  = ( freq :: GetSetAP
    | LowpassOptional
    )

defaultLowpass :: { | LowpassOptional }
defaultLowpass = { q: defaultGetSetAP 1.0 }

class LowpassCtor i lowpass | i -> lowpass where
  -- | Make a lowpass filter
  -- |
  -- | ```purescript
  -- | lowpass { freq: 440.0 } sinOsc 440.0
  -- | lowpass { freq: 440.0, q: 1.0 } sinOsc 440.0
  -- | lowpass 440.0 sinOsc 440.0
  -- | ```
  lowpass :: i -> lowpass

instance lowpassCtor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults Lowpass' { | LowpassOptional } { | provided } { | LowpassAll }
  ) =>
  LowpassCtor { | provided } (c -> CTOR.Lowpass GetSetAP GetSetAP c) where
  lowpass provided cont = CTOR.Lowpass all.freq all.q cont
    where
    all :: { | LowpassAll }
    all = convertOptionsWithDefaults Lowpass' defaultLowpass provided
else instance lowpassCtor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => LowpassCtor a (b -> (CTOR.Lowpass GetSetAP GetSetAP b)) where
  lowpass a cont = CTOR.Lowpass (Tuple (initialVal a) (setterVal a)) defaultLowpass.q cont

-----
data Lowshelf'
  = Lowshelf'

instance convertLowshelfFrequency :: (InitialVal a, SetterVal a) => ConvertOption Lowshelf' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertLowshelfQ :: (InitialVal a, SetterVal a) => ConvertOption Lowshelf' "gain" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type LowshelfOptional
  = ( gain :: GetSetAP )

type LowshelfAll
  = ( freq :: GetSetAP
    | LowshelfOptional
    )

defaultLowshelf :: { | LowshelfOptional }
defaultLowshelf = { gain: defaultGetSetAP 0.0 }

class LowshelfCtor i lowshelf | i -> lowshelf where
  -- | Make a lowshelf filter
  -- |
  -- | ```purescript
  -- | lowshelf { freq: 440.0 } sinOsc 440.0
  -- | lowshelf { freq: 440.0, gain: 1.0 } sinOsc 440.0
  -- | lowshelf 440.0 sinOsc 440.0
  -- | ```
  lowshelf :: i -> lowshelf

instance lowshelfCtor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults Lowshelf' { | LowshelfOptional } { | provided } { | LowshelfAll }
  ) =>
  LowshelfCtor { | provided } (c -> CTOR.Lowshelf GetSetAP GetSetAP c) where
  lowshelf provided cont = CTOR.Lowshelf all.freq all.gain cont
    where
    all :: { | LowshelfAll }
    all = convertOptionsWithDefaults Lowshelf' defaultLowshelf provided
else instance lowshelfCtor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => LowshelfCtor a (b -> (CTOR.Lowshelf GetSetAP GetSetAP b)) where
  lowshelf a cont = CTOR.Lowshelf (Tuple (initialVal a) (setterVal a)) defaultLowshelf.gain cont

--------
microphone :: CTOR.Microphone
microphone = CTOR.Microphone

--------
data Notch'
  = Notch'

instance convertNotchFrequency :: (InitialVal a, SetterVal a) => ConvertOption Notch' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertNotchQ :: (InitialVal a, SetterVal a) => ConvertOption Notch' "q" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type NotchOptional
  = ( q :: GetSetAP )

type NotchAll
  = ( freq :: GetSetAP
    | NotchOptional
    )

defaultNotch :: { | NotchOptional }
defaultNotch = { q: defaultGetSetAP 1.0 }

class NotchCtor i notch | i -> notch where
  notch :: i -> notch

instance notchCtor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults Notch' { | NotchOptional } { | provided } { | NotchAll }
  ) =>
  NotchCtor { | provided } (c -> CTOR.Notch GetSetAP GetSetAP c) where
  notch provided cont = CTOR.Notch all.freq all.q cont
    where
    all :: { | NotchAll }
    all = convertOptionsWithDefaults Notch' defaultNotch provided
else instance notchCtor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => NotchCtor a (b -> (CTOR.Notch GetSetAP GetSetAP b)) where
  notch a cont = CTOR.Notch (Tuple (initialVal a) (setterVal a)) defaultNotch.q cont

data Peaking'
  = Peaking'

instance convertPeakingFrequency :: (InitialVal a, SetterVal a) => ConvertOption Peaking' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertPeakingQ :: (InitialVal a, SetterVal a) => ConvertOption Peaking' "q" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertPeakingGain :: (InitialVal a, SetterVal a) => ConvertOption Peaking' "gain" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type PeakingOptional
  = ( q :: GetSetAP, gain :: GetSetAP )

type PeakingAll
  = ( freq :: GetSetAP
    | PeakingOptional
    )

defaultPeaking :: { | PeakingOptional }
defaultPeaking = { q: defaultGetSetAP 1.0, gain: defaultGetSetAP 0.0 }

class PeakingCtor i peaking | i -> peaking where
  peaking :: i -> peaking

instance peakingCtor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults Peaking' { | PeakingOptional } { | provided } { | PeakingAll }
  ) =>
  PeakingCtor { | provided } (c -> CTOR.Peaking GetSetAP GetSetAP GetSetAP c) where
  peaking provided cont = CTOR.Peaking all.freq all.q all.gain cont
    where
    all :: { | PeakingAll }
    all = convertOptionsWithDefaults Peaking' defaultPeaking provided
else instance peakingCtor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => PeakingCtor a (b -> (CTOR.Peaking GetSetAP GetSetAP GetSetAP b)) where
  peaking a cont = CTOR.Peaking (Tuple (initialVal a) (setterVal a)) defaultPeaking.q defaultPeaking.gain cont

------
class PeriodicOscCtor i o | i -> o where
  -- | Make a periodic oscillator
  -- |
  -- | ```purescript
  -- | periodicOsc (Proxy :: _ "my-wavetable") 440.0
  -- | periodicOsc On (Proxy :: _ "my-wavetable") 440.0
  -- | ```
  periodicOsc :: i -> o

instance periodicOsc1 ::
  ( InitialVal a
  , IsSymbol s
  , SetterVal a
  ) =>
  PeriodicOscCtor (Proxy s) (a -> CTOR.PeriodicOsc s GetSetAP) where
  periodicOsc px gvsv = CTOR.PeriodicOsc px On (Tuple (initialVal gvsv) (setterVal gvsv))

instance periodicOsc2 ::
  ( InitialVal a
  , IsSymbol s
  , SetterVal a
  ) =>
  PeriodicOscCtor OnOff (Proxy s -> a -> CTOR.PeriodicOsc s GetSetAP) where
  periodicOsc oo px gvsv = CTOR.PeriodicOsc px oo (Tuple (initialVal gvsv) (setterVal gvsv))

---
data PlayBuf'
  = PlayBuf'

instance convertPlayBufPlaybackRate :: (InitialVal a, SetterVal a) => ConvertOption PlayBuf' "playbackRate" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertPlayBufOnOff :: ConvertOption PlayBuf' "onOff" OnOff OnOff where
  convertOption _ _ = identity

type PlayBufOptional
  = ( playbackRate :: GetSetAP, onOff :: OnOff, start :: Number )

type PlayBufAll
  = ( | PlayBufOptional )

defaultPlayBuf :: { | PlayBufOptional }
defaultPlayBuf = { playbackRate: defaultGetSetAP 1.0, onOff: On, start: 0.0 }

class PlayBufCtor i playBuf | i -> playBuf where
  -- | Make a unit that plays from a buffer.
  -- |
  -- | ```purescript
  -- | playBuf { playbackRate: 1.0 } (Proxy :: _ "track")
  -- | playBuf { playbackRate: 1.0, start: 0.5 } (Proxy :: _ "track")
  -- | playBuf (Proxy :: _ "track")
  -- | ```
  playBuf :: i -> playBuf

instance playBufCtor1 ::
  ( IsSymbol l
  , ConvertOptionsWithDefaults PlayBuf' { | PlayBufOptional } { | provided } { | PlayBufAll }
  ) =>
  PlayBufCtor { | provided } (Proxy l -> CTOR.PlayBuf l GetSetAP) where
  playBuf provided proxy = CTOR.PlayBuf proxy all.start all.onOff all.playbackRate
    where
    all :: { | PlayBufAll }
    all = convertOptionsWithDefaults PlayBuf' defaultPlayBuf provided
else instance playBufCtor2 :: IsSymbol l => PlayBufCtor (Proxy l) (CTOR.PlayBuf l GetSetAP) where
  playBuf proxy =
    CTOR.PlayBuf
      proxy
      defaultPlayBuf.start
      defaultPlayBuf.onOff
      defaultPlayBuf.playbackRate

------
-- | Make a recorder.
-- |
-- | ```purescript
-- | recorder (Proxy :: _ "track") music
-- | ```
recorder ::
  forall a c.
  IsSymbol a =>
  IsAudio c =>
  Proxy a -> c -> CTOR.Recorder a c
recorder = CTOR.Recorder

------
class SawtoothOscCtor i o | i -> o where
  -- | Make a sawtooth oscillator
  -- |
  -- | ```purescript
  -- | sawtoothOsc 440.0
  -- | sawtoothOsc On 440.0
  -- | ```
  sawtoothOsc :: i -> o

instance sawtoothOsc2 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SawtoothOscCtor OnOff (a -> CTOR.SawtoothOsc GetSetAP) where
  sawtoothOsc oo gvsv = CTOR.SawtoothOsc oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance sawtoothOsc1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SawtoothOscCtor a (CTOR.SawtoothOsc GetSetAP) where
  sawtoothOsc gvsv = CTOR.SawtoothOsc On (Tuple (initialVal gvsv) (setterVal gvsv))

------
class SinOscCtor i o | i -> o where
  -- | Make a sine-wave oscillator
  -- |
  -- | ```purescript
  -- | sinOsc 440.0
  -- | sinOsc On 440.0
  -- | ```
  sinOsc :: i -> o

instance sinOscCtor2 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SinOscCtor OnOff (a -> CTOR.SinOsc GetSetAP) where
  sinOsc oo gvsv = CTOR.SinOsc oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance sinOscCtor1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SinOscCtor a (CTOR.SinOsc GetSetAP) where
  sinOsc gvsv = CTOR.SinOsc On (Tuple (initialVal gvsv) (setterVal gvsv))

------
-- | Send sound to the loudspeaker.
-- |
-- | ```purescript
-- | speaker music
-- | ```
speaker ::
  forall c.
  IsMultiAudio c =>
  c -> CTOR.Speaker c
speaker = CTOR.Speaker

------
class SquareOscCtor i o | i -> o where
  -- | Make a square-wave oscillator
  -- |
  -- | ```purescript
  -- | squareOsc 440.0
  -- | squareOsc On 440.0
  -- | ```
  squareOsc :: i -> o

instance squareOscCtor2 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SquareOscCtor OnOff (a -> CTOR.SquareOsc GetSetAP) where
  squareOsc oo gvsv = CTOR.SquareOsc oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance squareOscCtor1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SquareOscCtor a (CTOR.SquareOsc GetSetAP) where
  squareOsc gvsv = CTOR.SquareOsc On (Tuple (initialVal gvsv) (setterVal gvsv))

------
-- | Pan audio.
-- |
-- | ```purescript
-- | pan 0.5 music
-- | ```
pan ::
  forall a c.
  InitialVal a =>
  SetterVal a =>
  IsAudioOrF c =>
  a -> c -> CTOR.StereoPanner GetSetAP c
pan gvsv cont = CTOR.StereoPanner (Tuple (initialVal gvsv) (setterVal gvsv)) cont

------
class TriangleOscCtor i o | i -> o where
  -- | Make a triangle-wave oscillator
  -- |
  -- | ```purescript
  -- | triangleOsc 440.0
  -- | triangleOsc On 440.0
  -- | ```
  triangleOsc :: i -> o

instance triangleOscCtor2 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  TriangleOscCtor OnOff (a -> CTOR.TriangleOsc GetSetAP) where
  triangleOsc oo gvsv = CTOR.TriangleOsc oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance triangleOscCtor1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  TriangleOscCtor a (CTOR.TriangleOsc GetSetAP) where
  triangleOsc gvsv = CTOR.TriangleOsc On (Tuple (initialVal gvsv) (setterVal gvsv))

----------
-- | Apply distorion to audio
-- |
-- | ```purescript
-- | waveShaper (Proxy :: _ "my-wave") OversampleNone sound
-- | ```
waveShaper ::
  forall a b c.
  IsSymbol a =>
  IsOversample b =>
  IsAudioOrF c =>
  Proxy a -> b -> c -> (CTOR.WaveShaper a b c)
waveShaper = CTOR.WaveShaper
