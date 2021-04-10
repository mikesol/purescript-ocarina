module WAGS.Graph.Optionals where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy)
import WAGS.Change (class SetterVal, setterVal)
import WAGS.Create (class InitialVal, initialVal)
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.IsAudio (class IsAudio, class IsAudioOrF, class IsMultiAudio, class IsMultiAudioOrF, class IsOversample)
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
constant ::
  forall a.
  InitialVal a =>
  SetterVal a =>
  a -> CTOR.Constant GetSetAP
constant gvsv = CTOR.Constant (Tuple (initialVal gvsv) (setterVal gvsv))

------
convolver ::
  forall s c.
  IsSymbol s =>
  IsAudioOrF c =>
  Proxy s -> c -> CTOR.Convolver s c
convolver px cont = CTOR.Convolver px cont

------
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
  gain :: i -> gain

instance gainCtor1 :: (InitialVal a, SetterVal a, IsMultiAudioOrF b) => GainCtor a (b -> (CTOR.Gain GetSetAP b)) where
  gain a cont = CTOR.Gain (Tuple (initialVal a) (setterVal a)) cont

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
class LoopBufCtor loopBuf where
  loopBuf :: loopBuf

instance loopBufCtor1 :: (IsSymbol s, InitialVal a, SetterVal a) => LoopBufCtor (Proxy s -> a -> CTOR.LoopBuf s GetSetAP) where
  loopBuf s a = CTOR.LoopBuf s (Tuple (initialVal a) (setterVal a)) 0.0 0.0

instance loopBufCtor2 :: (IsSymbol s, InitialVal a, SetterVal a) => LoopBufCtor (Proxy s -> a -> Number -> CTOR.LoopBuf s GetSetAP) where
  loopBuf s a start = CTOR.LoopBuf s (Tuple (initialVal a) (setterVal a)) start 0.0

instance loopBufCtor3 :: (IsSymbol s, InitialVal a, SetterVal a) => LoopBufCtor (Proxy s -> a -> Number -> Number -> CTOR.LoopBuf s GetSetAP) where
  loopBuf s a start end = CTOR.LoopBuf s (Tuple (initialVal a) (setterVal a)) start end

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
periodicOsc ::
  forall s a.
  IsSymbol s =>
  InitialVal a =>
  SetterVal a =>
  Proxy s -> a -> CTOR.PeriodicOsc s GetSetAP
periodicOsc px gvsv = CTOR.PeriodicOsc px (Tuple (initialVal gvsv) (setterVal gvsv))

---
class PlayBufCtor playBuf where
  playBuf :: playBuf

instance playBufCtor1 :: (IsSymbol s, InitialVal a, SetterVal a) => PlayBufCtor (Proxy s -> a -> CTOR.PlayBuf s GetSetAP) where
  playBuf s a = CTOR.PlayBuf s 0.0 (Tuple (initialVal a) (setterVal a))

instance playBufCtor2 :: (IsSymbol s, InitialVal a, SetterVal a) => PlayBufCtor ((Tuple (Proxy s) Number) -> a -> CTOR.PlayBuf s GetSetAP) where
  playBuf (Tuple s offset) a = CTOR.PlayBuf s offset (Tuple (initialVal a) (setterVal a))

------
recorder ::
  forall a c.
  IsSymbol a =>
  IsAudio c =>
  Proxy a -> c -> CTOR.Recorder a c
recorder = CTOR.Recorder

------
sawtoothOsc ::
  forall a.
  InitialVal a =>
  SetterVal a =>
  a -> CTOR.SawtoothOsc GetSetAP
sawtoothOsc gvsv = CTOR.SawtoothOsc (Tuple (initialVal gvsv) (setterVal gvsv))

------
sinOsc ::
  forall a.
  InitialVal a =>
  SetterVal a =>
  a -> CTOR.SinOsc GetSetAP
sinOsc gvsv = CTOR.SinOsc (Tuple (initialVal gvsv) (setterVal gvsv))

------
speaker ::
  forall c.
  IsMultiAudio c =>
  c -> CTOR.Speaker c
speaker = CTOR.Speaker

------
squareOsc ::
  forall a.
  InitialVal a =>
  SetterVal a =>
  a -> CTOR.SquareOsc GetSetAP
squareOsc gvsv = CTOR.SquareOsc (Tuple (initialVal gvsv) (setterVal gvsv))

------
pan ::
  forall a c.
  InitialVal a =>
  SetterVal a =>
  IsAudioOrF c =>
  a -> c -> CTOR.StereoPanner GetSetAP c
pan gvsv cont = CTOR.StereoPanner (Tuple (initialVal gvsv) (setterVal gvsv)) cont


------
triangleOsc ::
  forall a.
  InitialVal a =>
  SetterVal a =>
  a -> CTOR.TriangleOsc GetSetAP
triangleOsc gvsv = CTOR.TriangleOsc (Tuple (initialVal gvsv) (setterVal gvsv))

----------
waveShaper ::
  forall a b c.
  IsSymbol a =>
  IsOversample b =>
  IsAudioOrF c =>
  Proxy a -> b -> c -> (CTOR.WaveShaper a b c)
waveShaper = CTOR.WaveShaper