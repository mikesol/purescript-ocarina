-- | This module provides functions for the construction of audio units that more closely resemble the overloaded constructors of the Web Audio API.
module WAGS.Graph.Optionals where

import Prelude
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec as V
import Type.Proxy (Proxy)
import WAGS.Change (class SetterVal, setterVal)
import WAGS.Create (class InitialVal, initialVal)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Oversample (class IsOversample)
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
  -- | Create an allpass filter, connecting it to another unit
  -- |
  -- | ```purescript
  -- | allpass { freq: 440.0 } { sinOsc: unit }
  -- | allpass { freq: 440.0, q: 1.0 } { sinOsc: unit }
  -- | allpass 440.0 { sinOsc: unit }
  -- | ```
  allpass :: i -> allpass

instance allpassCtor1 ::
  ( ConvertOptionsWithDefaults Allpass' { | AllpassOptional } { | provided } { | AllpassAll }
    ) =>
  AllpassCtor { | provided } (b -> CTOR.Allpass GetSetAP GetSetAP /\ b) where
  allpass provided b = CTOR.Allpass all.freq all.q /\ b
    where
    all :: { | AllpassAll }
    all = convertOptionsWithDefaults Allpass' defaultAllpass provided
else instance allpassCtor2 :: (InitialVal a, SetterVal a) => AllpassCtor a (b -> CTOR.Allpass GetSetAP GetSetAP /\ b) where
  allpass a b = CTOR.Allpass (Tuple (initialVal a) (setterVal a)) defaultAllpass.q /\ b

class AllpassCtor_ i allpass | i -> allpass where
  -- | Change an allpass filter
  -- |
  -- | ```purescript
  -- | allpass_ { freq: 440.0 }
  -- | allpass_ { freq: 440.0, q: 1.0 }
  -- | allpass_ 440.0
  -- | ```
  allpass_ :: i -> allpass

instance allpassCtor1_ ::
  ( ConvertOptionsWithDefaults Allpass' { | AllpassOptional } { | provided } { | AllpassAll }
    ) =>
  AllpassCtor_ { | provided } (CTOR.Allpass GetSetAP GetSetAP) where
  allpass_ provided = CTOR.Allpass all.freq all.q
    where
    all :: { | AllpassAll }
    all = convertOptionsWithDefaults Allpass' defaultAllpass provided
else instance allpassCtor2_ :: (InitialVal a, SetterVal a) => AllpassCtor_ a (CTOR.Allpass GetSetAP GetSetAP) where
  allpass_ a = CTOR.Allpass (Tuple (initialVal a) (setterVal a)) defaultAllpass.q

-- | The type of a created allpass filter
type CAllpass a
  = CTOR.Allpass GetSetAP GetSetAP /\ a

-- | The type of a changing allpass filter
type DAllpass
  = CTOR.Allpass GetSetAP GetSetAP

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
  -- | Create a bandpass filter, connecting it to another unit
  -- |
  -- | ```purescript
  -- | bandpass { freq: 440.0 } { sinOsc: unit }
  -- | bandpass { freq: 440.0, q: 1.0 } { sinOsc: unit }
  -- | bandpass 440.0 { sinOsc: unit }
  -- | ```
  bandpass :: i -> bandpass

instance bandpassCtor1 ::
  ( ConvertOptionsWithDefaults Bandpass' { | BandpassOptional } { | provided } { | BandpassAll }
    ) =>
  BandpassCtor { | provided } (b -> CTOR.Bandpass GetSetAP GetSetAP /\ b) where
  bandpass provided b = CTOR.Bandpass all.freq all.q /\ b
    where
    all :: { | BandpassAll }
    all = convertOptionsWithDefaults Bandpass' defaultBandpass provided
else instance bandpassCtor2 :: (InitialVal a, SetterVal a) => BandpassCtor a (b -> CTOR.Bandpass GetSetAP GetSetAP /\ b) where
  bandpass a b = CTOR.Bandpass (Tuple (initialVal a) (setterVal a)) defaultBandpass.q /\ b

class BandpassCtor_ i bandpass | i -> bandpass where
  -- | Change a bandpass filter
  -- |
  -- | ```purescript
  -- | bandpass_ { freq: 440.0 }
  -- | bandpass_ { freq: 440.0, q: 1.0 }
  -- | bandpass_ 440.0
  -- | ```
  bandpass_ :: i -> bandpass

instance bandpass_Ctor1 ::
  ( ConvertOptionsWithDefaults Bandpass' { | BandpassOptional } { | provided } { | BandpassAll }
    ) =>
  BandpassCtor_ { | provided } (CTOR.Bandpass GetSetAP GetSetAP) where
  bandpass_ provided = CTOR.Bandpass all.freq all.q
    where
    all :: { | BandpassAll }
    all = convertOptionsWithDefaults Bandpass' defaultBandpass provided
else instance bandpass_Ctor2 :: (InitialVal a, SetterVal a) => BandpassCtor_ a ((CTOR.Bandpass GetSetAP GetSetAP)) where
  bandpass_ a = CTOR.Bandpass (Tuple (initialVal a) (setterVal a)) defaultBandpass.q

type CBandpass a
  = CTOR.Bandpass GetSetAP GetSetAP /\ a

type DBandpass
  = CTOR.Bandpass GetSetAP GetSetAP

------
class ConstantCtor_ i o | i -> o where
  -- | Change a constant value
  -- |
  -- | ```purescript
  -- | constant_ 0.5
  -- | constant_ On 0.5
  -- | ```
  constant_ :: i -> o

instance constantCtor2_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  ConstantCtor_ OnOff (a -> CTOR.Constant GetSetAP) where
  constant_ oo gvsv = CTOR.Constant oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance constantCtor1_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  ConstantCtor_ a (CTOR.Constant GetSetAP) where
  constant_ gvsv = CTOR.Constant On (Tuple (initialVal gvsv) (setterVal gvsv))

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
  ConstantCtor OnOff (a -> CTOR.Constant GetSetAP /\ {}) where
  constant oo gvsv = CTOR.Constant oo (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}
else instance constantCtor1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  ConstantCtor a (CTOR.Constant GetSetAP /\ {}) where
  constant gvsv = CTOR.Constant On (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}

type CConstant
  = CTOR.Constant GetSetAP /\ {}

type DConstant
  = CTOR.Constant GetSetAP

------
-- | Make a convolver, aka reverb.
-- |
-- | ```purescript
-- | convolver (Proxy :: _ "room") (playBuf "track")
-- | ```
convolver ::
  forall s b.
  IsSymbol s =>
  Proxy s -> b -> CTOR.Convolver s /\ b
convolver = Tuple <<< CTOR.Convolver

type CConvolver a b
  = CTOR.Convolver a /\ b

------
-- | Make a delay unit.
-- |
-- | ```purescript
-- | delay 0.5 (playBuf "track")
-- | ```
delay ::
  forall a b.
  InitialVal a =>
  SetterVal a =>
  a -> b -> CTOR.Delay GetSetAP /\ b
delay gvsv = Tuple (CTOR.Delay (Tuple (initialVal gvsv) (setterVal gvsv)))

delay_ :: forall a. InitialVal a => SetterVal a => a -> CTOR.Delay GetSetAP
delay_ = fst <<< flip delay {}

type CDelay a
  = CTOR.Delay GetSetAP /\ a

type DDelay
  = CTOR.Delay GetSetAP

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

class DynamicsCompressorCtor_ i compressor | i -> compressor where
  -- | Change a compressor.
  -- |
  -- | ```purescript
  -- | compressor_ { threshold: -10.0 }
  -- | compressor_ { knee: 20.0, ratio: 10.0 }
  -- | compressor_ { attack: 0.01, release: 0.3 }
  -- | ```
  compressor_ :: i -> compressor

instance compressorCTor_ ::
  ( ConvertOptionsWithDefaults DynamicsCompressor' { | DynamicsCompressorOptional } { | provided } { | DynamicsCompressorAll }
    ) =>
  DynamicsCompressorCtor_ { | provided } (CTOR.DynamicsCompressor GetSetAP GetSetAP GetSetAP GetSetAP GetSetAP) where
  compressor_ provided =
    CTOR.DynamicsCompressor
      all.threshold
      all.knee
      all.ratio
      all.attack
      all.release
    where
    all :: { | DynamicsCompressorAll }
    all = convertOptionsWithDefaults DynamicsCompressor' defaultDynamicsCompressor provided

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
  ( ConvertOptionsWithDefaults DynamicsCompressor' { | DynamicsCompressorOptional } { | provided } { | DynamicsCompressorAll }
    ) =>
  DynamicsCompressorCtor { | provided } (b -> CTOR.DynamicsCompressor GetSetAP GetSetAP GetSetAP GetSetAP GetSetAP /\ b) where
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
    all = convertOptionsWithDefaults DynamicsCompressor' defaultDynamicsCompressor provided

type CDynamicsCompressor a
  = CTOR.DynamicsCompressor GetSetAP GetSetAP GetSetAP GetSetAP GetSetAP /\ a

type DDynamicsCompressor
  = CTOR.DynamicsCompressor GetSetAP GetSetAP GetSetAP GetSetAP GetSetAP

------
gain :: forall a b. InitialVal a => SetterVal a => a -> b -> CTOR.Gain GetSetAP /\ b
gain a = Tuple (CTOR.Gain (Tuple (initialVal a) (setterVal a)))

gain_ :: forall a. InitialVal a => SetterVal a => a -> CTOR.Gain GetSetAP
gain_ = fst <<< flip gain {}

-- | Mix together several audio units
-- |
-- | ```purescript
-- | mix (playBuf (Proxy :: _ "hello") /\ playBuf (Proxy :: _ "world") /\ unit)
-- | ```
mix :: forall a. a -> CTOR.Gain GetSetAP /\ a
mix = Tuple (CTOR.Gain (defaultGetSetAP 1.0))

type Mix
  = CTOR.Gain GetSetAP

type CGain a
  = CTOR.Gain GetSetAP /\ a

type DGain
  = CTOR.Gain GetSetAP

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

class HighpassCtor_ i highpass | i -> highpass where
  -- | Change a highpass filter
  -- |
  -- | ```purescript
  -- | highpass_ { freq: 440.0 } 
  -- | highpass_ { freq: 440.0, q: 1.0 } 
  -- | highpass_ 440.0 
  -- | ```
  highpass_ :: i -> highpass

instance highpassCtor1_ ::
  ( ConvertOptionsWithDefaults Highpass' { | HighpassOptional } { | provided } { | HighpassAll }
    ) =>
  HighpassCtor_ { | provided } (CTOR.Highpass GetSetAP GetSetAP) where
  highpass_ provided = CTOR.Highpass all.freq all.q
    where
    all :: { | HighpassAll }
    all = convertOptionsWithDefaults Highpass' defaultHighpass provided
else instance highpassCtor2_ :: (InitialVal a, SetterVal a) => HighpassCtor_ a ((CTOR.Highpass GetSetAP GetSetAP)) where
  highpass_ a = CTOR.Highpass (Tuple (initialVal a) (setterVal a)) defaultHighpass.q

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
  ( ConvertOptionsWithDefaults Highpass' { | HighpassOptional } { | provided } { | HighpassAll }
    ) =>
  HighpassCtor { | provided } (b -> CTOR.Highpass GetSetAP GetSetAP /\ b) where
  highpass provided = Tuple (CTOR.Highpass all.freq all.q)
    where
    all :: { | HighpassAll }
    all = convertOptionsWithDefaults Highpass' defaultHighpass provided
else instance highpassCtor2 :: (InitialVal a, SetterVal a) => HighpassCtor a (b -> CTOR.Highpass GetSetAP GetSetAP /\ b) where
  highpass a = Tuple (CTOR.Highpass (Tuple (initialVal a) (setterVal a)) defaultHighpass.q)

type CHighpass a
  = CTOR.Highpass GetSetAP GetSetAP /\ a

type DHighpass
  = CTOR.Highpass GetSetAP GetSetAP

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

class HighshelfCtor_ i highshelf | i -> highshelf where
  -- | Change a highshelf filter
  -- |
  -- | ```purescript
  -- | highshelf_ { freq: 440.0 }
  -- | highshelf_ { freq: 440.0, gain: 1.0 } 
  -- | highshelf_ 440.0 
  -- | ```
  highshelf_ :: i -> highshelf

instance highshelfCtor1_ ::
  ( ConvertOptionsWithDefaults Highshelf' { | HighshelfOptional } { | provided } { | HighshelfAll }
    ) =>
  HighshelfCtor_ { | provided } (CTOR.Highshelf GetSetAP GetSetAP) where
  highshelf_ provided = CTOR.Highshelf all.freq all.gain
    where
    all :: { | HighshelfAll }
    all = convertOptionsWithDefaults Highshelf' defaultHighshelf provided
else instance highshelfCtor2_ :: (InitialVal a, SetterVal a) => HighshelfCtor_ a ((CTOR.Highshelf GetSetAP GetSetAP)) where
  highshelf_ a = CTOR.Highshelf (Tuple (initialVal a) (setterVal a)) defaultHighshelf.gain

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
  ( ConvertOptionsWithDefaults Highshelf' { | HighshelfOptional } { | provided } { | HighshelfAll }
    ) =>
  HighshelfCtor { | provided } (b -> CTOR.Highshelf GetSetAP GetSetAP /\ b) where
  highshelf provided = Tuple (CTOR.Highshelf all.freq all.gain)
    where
    all :: { | HighshelfAll }
    all = convertOptionsWithDefaults Highshelf' defaultHighshelf provided
else instance highshelfCtor2 :: (InitialVal a, SetterVal a) => HighshelfCtor a (b -> CTOR.Highshelf GetSetAP GetSetAP /\ b) where
  highshelf a = Tuple (CTOR.Highshelf (Tuple (initialVal a) (setterVal a)) defaultHighshelf.gain)

type CHighshelf a
  = CTOR.Highshelf GetSetAP GetSetAP /\ a

type DHighshelf
  = CTOR.Highshelf GetSetAP GetSetAP

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

class LoopBufCtor_ i loopBuf | i -> loopBuf where
  -- | Chage a looping buffer.
  -- |
  -- | ```purescript
  -- | loopBuf_ { playbackRate: 1.0 } "track"
  -- | loopBuf_ { playbackRate: 1.0, start: 0.5 } "track"
  -- | loopBuf_ "track"
  -- | ```
  loopBuf_ :: i -> loopBuf

instance loopBufCtor1_ ::
  ( ConvertOptionsWithDefaults LoopBuf' { | LoopBufOptional } { | provided } { | LoopBufAll }
    ) =>
  LoopBufCtor_ { | provided } (String -> CTOR.LoopBuf GetSetAP) where
  loopBuf_ provided proxy = CTOR.LoopBuf proxy all.onOff all.playbackRate all.start all.end
    where
    all :: { | LoopBufAll }
    all = convertOptionsWithDefaults LoopBuf' defaultLoopBuf provided
else instance loopBufCtor2_ :: LoopBufCtor_ String (CTOR.LoopBuf GetSetAP) where
  loopBuf_ name =
    CTOR.LoopBuf
      name
      defaultLoopBuf.onOff
      defaultLoopBuf.playbackRate
      defaultLoopBuf.start
      defaultLoopBuf.end

class LoopBufCtor i loopBuf | i -> loopBuf where
  -- | Make a looping buffer.
  -- |
  -- | ```purescript
  -- | loopBuf { playbackRate: 1.0 } "track"
  -- | loopBuf { playbackRate: 1.0, start: 0.5 } "track"
  -- | loopBuf "track"
  -- | ```
  loopBuf :: i -> loopBuf

instance loopBufCtor1 ::
  ( ConvertOptionsWithDefaults LoopBuf' { | LoopBufOptional } { | provided } { | LoopBufAll }
    ) =>
  LoopBufCtor { | provided } (String -> CTOR.LoopBuf GetSetAP /\ {}) where
  loopBuf provided proxy = CTOR.LoopBuf proxy all.onOff all.playbackRate all.start all.end /\ {}
    where
    all :: { | LoopBufAll }
    all = convertOptionsWithDefaults LoopBuf' defaultLoopBuf provided
else instance loopBufCtor2 :: LoopBufCtor String (CTOR.LoopBuf GetSetAP /\ {}) where
  loopBuf name =
    CTOR.LoopBuf
      name
      defaultLoopBuf.onOff
      defaultLoopBuf.playbackRate
      defaultLoopBuf.start
      defaultLoopBuf.end
      /\ {}

type CLoopBuf
  = CTOR.LoopBuf GetSetAP /\ {}

type DLoopBuf
  = CTOR.LoopBuf GetSetAP

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

class LowpassCtor_ i lowpass | i -> lowpass where
  -- | Change a lowpass filter
  -- |
  -- | ```purescript
  -- | lowpass_ { freq: 440.0 }
  -- | lowpass_ { freq: 440.0, q: 1.0 }
  -- | lowpass_ 440.0
  -- | ```
  lowpass_ :: i -> lowpass

instance lowpass_Ctor1 ::
  ( ConvertOptionsWithDefaults Lowpass' { | LowpassOptional } { | provided } { | LowpassAll }
    ) =>
  LowpassCtor_ { | provided } (CTOR.Lowpass GetSetAP GetSetAP) where
  lowpass_ provided = CTOR.Lowpass all.freq all.q
    where
    all :: { | LowpassAll }
    all = convertOptionsWithDefaults Lowpass' defaultLowpass provided
else instance lowpass_Ctor2 :: (InitialVal a, SetterVal a) => LowpassCtor_ a ((CTOR.Lowpass GetSetAP GetSetAP)) where
  lowpass_ a = CTOR.Lowpass (Tuple (initialVal a) (setterVal a)) defaultLowpass.q

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
  ( ConvertOptionsWithDefaults Lowpass' { | LowpassOptional } { | provided } { | LowpassAll }
    ) =>
  LowpassCtor { | provided } (b -> CTOR.Lowpass GetSetAP GetSetAP /\ b) where
  lowpass provided = Tuple (CTOR.Lowpass all.freq all.q)
    where
    all :: { | LowpassAll }
    all = convertOptionsWithDefaults Lowpass' defaultLowpass provided
else instance lowpassCtor2 :: (InitialVal a, SetterVal a) => LowpassCtor a (b -> CTOR.Lowpass GetSetAP GetSetAP /\ b) where
  lowpass a = Tuple (CTOR.Lowpass (Tuple (initialVal a) (setterVal a)) defaultLowpass.q)

type CLowpass a
  = CTOR.Lowpass GetSetAP GetSetAP /\ a

type DLowpass
  = CTOR.Lowpass GetSetAP GetSetAP

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

class LowshelfCtor_ i lowshelf | i -> lowshelf where
  -- | Change a lowshelf filter
  -- |
  -- | ```purescript
  -- | lowshelf_ { freq: 440.0 }
  -- | lowshelf_ { freq: 440.0, gain: 1.0 }
  -- | lowshelf_ 440.0
  -- | ```
  lowshelf_ :: i -> lowshelf

instance lowshelf_Ctor1 ::
  ( ConvertOptionsWithDefaults Lowshelf' { | LowshelfOptional } { | provided } { | LowshelfAll }
    ) =>
  LowshelfCtor_ { | provided } (CTOR.Lowshelf GetSetAP GetSetAP) where
  lowshelf_ provided = CTOR.Lowshelf all.freq all.gain
    where
    all :: { | LowshelfAll }
    all = convertOptionsWithDefaults Lowshelf' defaultLowshelf provided
else instance lowshelf_Ctor2 :: (InitialVal a, SetterVal a) => LowshelfCtor_ a ((CTOR.Lowshelf GetSetAP GetSetAP)) where
  lowshelf_ a = CTOR.Lowshelf (Tuple (initialVal a) (setterVal a)) defaultLowshelf.gain

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
  ( ConvertOptionsWithDefaults Lowshelf' { | LowshelfOptional } { | provided } { | LowshelfAll }
    ) =>
  LowshelfCtor { | provided } (b -> CTOR.Lowshelf GetSetAP GetSetAP /\ b) where
  lowshelf provided = Tuple (CTOR.Lowshelf all.freq all.gain)
    where
    all :: { | LowshelfAll }
    all = convertOptionsWithDefaults Lowshelf' defaultLowshelf provided
else instance lowshelfCtor2 :: (InitialVal a, SetterVal a) => LowshelfCtor a (b -> CTOR.Lowshelf GetSetAP GetSetAP /\ b) where
  lowshelf a = Tuple (CTOR.Lowshelf (Tuple (initialVal a) (setterVal a)) defaultLowshelf.gain)

type CLowshelf a
  = CTOR.Lowshelf GetSetAP GetSetAP /\ a

type DLowshelf
  = CTOR.Lowshelf GetSetAP GetSetAP

--------
microphone :: CTOR.Microphone /\ {}
microphone = CTOR.Microphone /\ {}

microphone_ :: CTOR.Microphone
microphone_ = CTOR.Microphone

type CMicrophone
  = CTOR.Microphone /\ {}

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

class NotchCtor_ i notch | i -> notch where
  -- | Change a notch (band-reject) filter
  -- |
  -- | ```purescript
  -- | notch_ { freq: 440.0 }
  -- | notch_ { freq: 440.0, gain: 1.0 }
  -- | notch_ 440.0
  -- | ```
  notch_ :: i -> notch

instance notch_Ctor1 ::
  ( ConvertOptionsWithDefaults Notch' { | NotchOptional } { | provided } { | NotchAll }
    ) =>
  NotchCtor_ { | provided } (CTOR.Notch GetSetAP GetSetAP) where
  notch_ provided = CTOR.Notch all.freq all.q
    where
    all :: { | NotchAll }
    all = convertOptionsWithDefaults Notch' defaultNotch provided
else instance notch_Ctor2 :: (InitialVal a, SetterVal a) => NotchCtor_ a ((CTOR.Notch GetSetAP GetSetAP)) where
  notch_ a = CTOR.Notch (Tuple (initialVal a) (setterVal a)) defaultNotch.q

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
  ( ConvertOptionsWithDefaults Notch' { | NotchOptional } { | provided } { | NotchAll }
    ) =>
  NotchCtor { | provided } (b -> CTOR.Notch GetSetAP GetSetAP /\ b) where
  notch provided = Tuple (CTOR.Notch all.freq all.q)
    where
    all :: { | NotchAll }
    all = convertOptionsWithDefaults Notch' defaultNotch provided
else instance notchCtor2 :: (InitialVal a, SetterVal a) => NotchCtor a (b -> CTOR.Notch GetSetAP GetSetAP /\ b) where
  notch a = Tuple (CTOR.Notch (Tuple (initialVal a) (setterVal a)) defaultNotch.q)

type CNotch a
  = CTOR.Notch GetSetAP GetSetAP /\ a

type DNotch
  = CTOR.Notch GetSetAP GetSetAP

----------------
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
  -- | Make a peaking filter
  -- |
  -- | ```purescript
  -- | peaking { freq: 440.0 } { osc: sinOsc 440.0 }
  -- | peaking { freq: 440.0, gain: 1.0 } { osc: sinOsc 440.0 }
  -- | peaking 440.0 { osc: sinOsc 440.0 }
  -- | ```
  peaking :: i -> peaking

instance peakingCtor1 ::
  ( ConvertOptionsWithDefaults Peaking' { | PeakingOptional } { | provided } { | PeakingAll }
    ) =>
  PeakingCtor { | provided } (b -> CTOR.Peaking GetSetAP GetSetAP GetSetAP /\ b) where
  peaking provided = Tuple (CTOR.Peaking all.freq all.q all.gain)
    where
    all :: { | PeakingAll }
    all = convertOptionsWithDefaults Peaking' defaultPeaking provided
else instance peakingCtor2 :: (InitialVal a, SetterVal a) => PeakingCtor a (b -> CTOR.Peaking GetSetAP GetSetAP GetSetAP /\ b) where
  peaking a = Tuple (CTOR.Peaking (Tuple (initialVal a) (setterVal a)) defaultPeaking.q defaultPeaking.gain)

class PeakingCtor_ i peaking | i -> peaking where
  -- | Change a peaking filter
  -- |
  -- | ```purescript
  -- | peaking_ { freq: 440.0 }
  -- | peaking_ { freq: 440.0, gain: 1.0 }
  -- | peaking_ 440.0
  -- | ```
  peaking_ :: i -> peaking

instance peaking_Ctor1 ::
  ( ConvertOptionsWithDefaults Peaking' { | PeakingOptional } { | provided } { | PeakingAll }
    ) =>
  PeakingCtor_ { | provided } (CTOR.Peaking GetSetAP GetSetAP GetSetAP) where
  peaking_ provided = CTOR.Peaking all.freq all.q all.gain
    where
    all :: { | PeakingAll }
    all = convertOptionsWithDefaults Peaking' defaultPeaking provided
else instance peaking_Ctor2 :: (InitialVal a, SetterVal a) => PeakingCtor_ a (CTOR.Peaking GetSetAP GetSetAP GetSetAP) where
  peaking_ a = CTOR.Peaking (Tuple (initialVal a) (setterVal a)) defaultPeaking.q defaultPeaking.gain

type CPeaking a
  = CTOR.Peaking GetSetAP GetSetAP GetSetAP /\ a

type DPeaking
  = CTOR.Peaking GetSetAP GetSetAP GetSetAP

------
class PeriodicOscCtor_ i o | i -> o where
  -- | Change a periodic oscillator
  -- |
  -- | ```purescript
  -- | periodicOsc_ "my-wavetable" 440.0
  -- | periodicOsc_ On "my-wavetable" 440.0
  -- | ```
  periodicOsc_ :: i -> o

instance periodicOsc1_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  PeriodicOscCtor_ String (a -> CTOR.PeriodicOsc String GetSetAP) where
  periodicOsc_ px gvsv = CTOR.PeriodicOsc px On (Tuple (initialVal gvsv) (setterVal gvsv))

instance periodicOsc2_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  PeriodicOscCtor_ (V.Vec size Number /\ V.Vec size Number) (a -> CTOR.PeriodicOsc (V.Vec size Number /\ V.Vec size Number) GetSetAP) where
  periodicOsc_ px gvsv = CTOR.PeriodicOsc px On (Tuple (initialVal gvsv) (setterVal gvsv))

class IsPoscable (isPoscable :: Type)

instance isPoscableString :: IsPoscable String

instance isPoscableV :: IsPoscable (V.Vec size Number /\ V.Vec size Number)

instance periodicOsc3_ ::
  ( InitialVal a
  , SetterVal a
  , IsPoscable isPoscable
  ) =>
  PeriodicOscCtor_ OnOff (isPoscable -> a -> CTOR.PeriodicOsc isPoscable GetSetAP) where
  periodicOsc_ oo px gvsv = CTOR.PeriodicOsc px oo (Tuple (initialVal gvsv) (setterVal gvsv))

class PeriodicOscCtor i o | i -> o where
  -- | Make a periodic oscillator
  -- |
  -- | ```purescript
  -- | periodicOsc "my-wavetable" 440.0
  -- | periodicOsc On "my-wavetable" 440.0
  -- | ```
  periodicOsc :: i -> o

instance periodicOsc1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  PeriodicOscCtor String (a -> CTOR.PeriodicOsc String GetSetAP /\ {}) where
  periodicOsc px gvsv = CTOR.PeriodicOsc px On (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}

instance periodicOsc2 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  PeriodicOscCtor (V.Vec size Number /\ V.Vec size Number) (a -> CTOR.PeriodicOsc (V.Vec size Number /\ V.Vec size Number) GetSetAP /\ {}) where
  periodicOsc px gvsv = CTOR.PeriodicOsc px On (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}

instance periodicOsc3 ::
  ( InitialVal a
  , SetterVal a
  , IsPoscable isPoscable
  ) =>
  PeriodicOscCtor OnOff (isPoscable -> a -> CTOR.PeriodicOsc isPoscable GetSetAP /\ {}) where
  periodicOsc oo px gvsv = CTOR.PeriodicOsc px oo (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}

type CPeriodicOsc periodicOsc
  = CTOR.PeriodicOsc periodicOsc GetSetAP /\ {}

type DPeriodicOsc periodicOsc
  = CTOR.PeriodicOsc periodicOsc GetSetAP

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

class PlayBufCtor_ i playBuf | i -> playBuf where
  -- | Change a unit that plays from a buffer.
  -- |
  -- | ```purescript
  -- | playBuf_ { playbackRate: 1.0 } "track"
  -- | playBuf_ { playbackRate: 1.0, start: 0.5 } "track"
  -- | playBuf_ "track"
  -- | ```
  playBuf_ :: i -> playBuf

instance playBufCtor1_ ::
  ( ConvertOptionsWithDefaults PlayBuf' { | PlayBufOptional } { | provided } { | PlayBufAll }
    ) =>
  PlayBufCtor_ { | provided } (String -> CTOR.PlayBuf GetSetAP) where
  playBuf_ provided proxy = CTOR.PlayBuf proxy all.start all.onOff all.playbackRate
    where
    all :: { | PlayBufAll }
    all = convertOptionsWithDefaults PlayBuf' defaultPlayBuf provided
else instance playBufCtor2_ :: PlayBufCtor_ String (CTOR.PlayBuf GetSetAP) where
  playBuf_ str =
    CTOR.PlayBuf
      str
      defaultPlayBuf.start
      defaultPlayBuf.onOff
      defaultPlayBuf.playbackRate

class PlayBufCtor i playBuf | i -> playBuf where
  -- | Make a unit that plays from a buffer.
  -- |
  -- | ```purescript
  -- | playBuf { playbackRate: 1.0 } "track"
  -- | playBuf { playbackRate: 1.0, start: 0.5 } "track"
  -- | playBuf "track"
  -- | ```
  playBuf :: i -> playBuf

instance playBufCtor1 ::
  ConvertOptionsWithDefaults PlayBuf' { | PlayBufOptional } { | provided } { | PlayBufAll } =>
  PlayBufCtor { | provided } (String -> CTOR.PlayBuf GetSetAP /\ {}) where
  playBuf provided proxy = CTOR.PlayBuf proxy all.start all.onOff all.playbackRate /\ {}
    where
    all :: { | PlayBufAll }
    all = convertOptionsWithDefaults PlayBuf' defaultPlayBuf provided
else instance playBufCtor2 :: PlayBufCtor String (CTOR.PlayBuf GetSetAP /\ {}) where
  playBuf str =
    CTOR.PlayBuf
      str
      defaultPlayBuf.start
      defaultPlayBuf.onOff
      defaultPlayBuf.playbackRate
      /\ {}

type CPlayBuf
  = CTOR.PlayBuf GetSetAP /\ {}

type DPlayBuf
  = CTOR.PlayBuf GetSetAP

------
-- | Make a recorder.
-- |
-- | ```purescript
-- | recorder "track"
-- | ```
recorder ::
  forall a b.
  IsSymbol a =>
  Proxy a -> b -> CTOR.Recorder a /\ b
recorder = Tuple <<< CTOR.Recorder

type CRecorder a b
  = CTOR.Recorder a /\ b

------
class SawtoothOscCtor_ i o | i -> o where
  -- | Change a sawtooth oscillator
  -- |
  -- | ```purescript
  -- | sawtoothOsc_ 440.0
  -- | sawtoothOsc_ On 440.0
  -- | ```
  sawtoothOsc_ :: i -> o

instance sawtoothOsc2_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SawtoothOscCtor_ OnOff (a -> CTOR.SawtoothOsc GetSetAP) where
  sawtoothOsc_ oo gvsv = CTOR.SawtoothOsc oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance sawtoothOsc1_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SawtoothOscCtor_ a (CTOR.SawtoothOsc GetSetAP) where
  sawtoothOsc_ gvsv = CTOR.SawtoothOsc On (Tuple (initialVal gvsv) (setterVal gvsv))

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
  SawtoothOscCtor OnOff (a -> CTOR.SawtoothOsc GetSetAP /\ {}) where
  sawtoothOsc oo gvsv = CTOR.SawtoothOsc oo (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}
else instance sawtoothOsc1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SawtoothOscCtor a (CTOR.SawtoothOsc GetSetAP /\ {}) where
  sawtoothOsc gvsv = CTOR.SawtoothOsc On (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}

type CSawtoothOsc
  = CTOR.SawtoothOsc GetSetAP /\ {}

type DSawtoothOsc
  = CTOR.SawtoothOsc GetSetAP

------
class SinOscCtor_ i o | i -> o where
  -- | Change a sine-wave oscillator
  -- |
  -- | ```purescript
  -- | sinOsc_ 440.0
  -- | sinOsc_ On 440.0
  -- | ```
  sinOsc_ :: i -> o

instance sinOscCtor2_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SinOscCtor_ OnOff (a -> CTOR.SinOsc GetSetAP) where
  sinOsc_ oo gvsv = CTOR.SinOsc oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance sinOscCtor1_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SinOscCtor_ a (CTOR.SinOsc GetSetAP) where
  sinOsc_ gvsv = CTOR.SinOsc On (Tuple (initialVal gvsv) (setterVal gvsv))

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
  SinOscCtor OnOff (a -> CTOR.SinOsc GetSetAP /\ {}) where
  sinOsc oo gvsv = CTOR.SinOsc oo (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}
else instance sinOscCtor1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SinOscCtor a (CTOR.SinOsc GetSetAP /\ {}) where
  sinOsc gvsv = CTOR.SinOsc On (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}

type CSinOsc
  = CTOR.SinOsc GetSetAP /\ {}

type DSinOsc
  = CTOR.SinOsc GetSetAP

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
class SquareOscCtor_ i o | i -> o where
  -- | Change a square-wave oscillator
  -- |
  -- | ```purescript
  -- | squareOsc_ 440.0
  -- | squareOsc_ On 440.0
  -- | ```
  squareOsc_ :: i -> o

instance squareOscCtor2_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SquareOscCtor_ OnOff (a -> CTOR.SquareOsc GetSetAP) where
  squareOsc_ oo gvsv = CTOR.SquareOsc oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance squareOscCtor1_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SquareOscCtor_ a (CTOR.SquareOsc GetSetAP) where
  squareOsc_ gvsv = CTOR.SquareOsc On (Tuple (initialVal gvsv) (setterVal gvsv))

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
  SquareOscCtor OnOff (a -> CTOR.SquareOsc GetSetAP /\ {}) where
  squareOsc oo gvsv = CTOR.SquareOsc oo (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}
else instance squareOscCtor1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  SquareOscCtor a (CTOR.SquareOsc GetSetAP /\ {}) where
  squareOsc gvsv = CTOR.SquareOsc On (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}

type CSquareOsc
  = CTOR.SquareOsc GetSetAP /\ {}

type DSquareOsc
  = CTOR.SquareOsc GetSetAP

------
-- | Pan audio.
-- |
-- | ```purescript
-- | pan 0.5 { buf: playBuf "my-track" }
-- | ```
pan ::
  forall a b.
  InitialVal a =>
  SetterVal a =>
  a -> b -> CTOR.StereoPanner GetSetAP /\ b
pan gvsv = Tuple (CTOR.StereoPanner (Tuple (initialVal gvsv) (setterVal gvsv)))

-- | Change panned audio.
-- |
-- | ```purescript
-- | pan_ (-0.33)
-- | ```
pan_ :: forall a. InitialVal a => SetterVal a => a -> CTOR.StereoPanner GetSetAP
pan_ = fst <<< flip pan {}

type CStereoPanner a
  = CTOR.StereoPanner GetSetAP /\ a

type DStereoPanner
  = CTOR.StereoPanner GetSetAP

------
class TriangleOscCtor_ i o | i -> o where
  -- | Change a triangle-wave oscillator
  -- |
  -- | ```purescript
  -- | triangleOsc_ 440.0
  -- | triangleOsc_ On 440.0
  -- | ```
  triangleOsc_ :: i -> o

instance triangleOscCtor2_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  TriangleOscCtor_ OnOff (a -> CTOR.TriangleOsc GetSetAP) where
  triangleOsc_ oo gvsv = CTOR.TriangleOsc oo (Tuple (initialVal gvsv) (setterVal gvsv))
else instance triangleOscCtor1_ ::
  ( InitialVal a
  , SetterVal a
  ) =>
  TriangleOscCtor_ a (CTOR.TriangleOsc GetSetAP) where
  triangleOsc_ gvsv = CTOR.TriangleOsc On (Tuple (initialVal gvsv) (setterVal gvsv))

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
  TriangleOscCtor OnOff (a -> CTOR.TriangleOsc GetSetAP /\ {}) where
  triangleOsc oo gvsv = CTOR.TriangleOsc oo (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}
else instance triangleOscCtor1 ::
  ( InitialVal a
  , SetterVal a
  ) =>
  TriangleOscCtor a (CTOR.TriangleOsc GetSetAP /\ {}) where
  triangleOsc gvsv = CTOR.TriangleOsc On (Tuple (initialVal gvsv) (setterVal gvsv)) /\ {}

type CTriangleOsc
  = CTOR.TriangleOsc GetSetAP /\ {}

type DTriangleOsc
  = CTOR.TriangleOsc GetSetAP

----------
-- | Apply distorion to audio
-- |
-- | ```purescript
-- | waveShaper (Proxy :: _ "my-wave") OversampleNone { buf: playBuf "my-track" }
-- | ```
waveShaper ::
  forall a b c.
  IsSymbol a =>
  IsOversample b =>
  Proxy a -> b -> c -> CTOR.WaveShaper a b /\ c
waveShaper a = Tuple <<< CTOR.WaveShaper a

waveShaper_ :: forall a b. IsSymbol a => IsOversample b => Proxy a -> b -> CTOR.WaveShaper a b
waveShaper_ a b = fst $ waveShaper a b {}

type CWaveShaper a b c
  = CTOR.WaveShaper a b /\ c

---------------
-- | A reference to a node in a graph.
type Ref
  = Unit /\ {}

-- | A reference to a node in a graph.
ref :: Ref
ref = unit /\ {}
