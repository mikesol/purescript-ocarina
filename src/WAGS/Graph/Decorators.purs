module WAGS.Graph.Decorators where

import Prelude
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Prim.Row (class Cons, class Lacks)
import Prim.RowList as RL
import Record (insert, modify)
import Type.Proxy (Proxy(..))
import Data.Identity (Identity)
import Data.Tuple (Tuple)
import Type.Proxy (Proxy)
import WAGS.Graph.Constructors as CTOR
import WAGS.Rendered (Oversample(..))

type Decorated a = forall b c. Decorate a b c => { | b } /\ { | c }

class Decorate (a :: Row Type) (b :: Row Type) (c :: Row Type) | a -> b c where
  decorate :: forall d. ({ | a } -> d) -> { | b } /\ { | c }

class MakeDecorators (rl :: RL.RowList Type) b | rl -> b where
  makeDecorators :: Proxy rl -> { | b }

instance makeDecoratorsRLCons :: (IsSymbol sym, Lacks sym rest, Cons sym (Decorating Identity) rest r, MakeDecorators b rest) => MakeDecorators (RL.Cons sym (Decorating f) b) r where
  makeDecorators _ = insert (Proxy :: _ sym) (Decorating Identity) (makeDecorators (Proxy :: _ b))

instance makeDecoratorsRLNil :: MakeDecorators RL.Nil () where
  makeDecorators _ = {}

class MakeFocusing (rl :: RL.RowList Type) (b :: Row Type) (c :: Row Type) | rl b -> c where
  makeFocusing :: Proxy rl -> { | b } -> { | c }

instance makeMakeFocusingRLCons :: (IsSymbol sym, Lacks sym rest, Cons sym (Decorating Identity) q r, Cons sym (Decorating Focus) q r', Cons sym (Record r') rest c, MakeFocusing b r rest) => MakeFocusing (RL.Cons sym (Decorating f) b) r c where
  makeFocusing _ r = insert (Proxy :: _ sym) (modify (Proxy :: Proxy sym) (\_ -> Decorating Focus) r) (makeFocusing (Proxy :: _ b) r)

instance makeFocusingNil :: MakeFocusing RL.Nil x () where
  makeFocusing _ _ = {}

instance decorateDecorating :: (RL.RowToList a rl, MakeDecorators rl b, MakeFocusing rl b c) => Decorate a b c where
  decorate _ = idy /\ (makeFocusing (Proxy :: _ rl) idy)
    where
    idy = makeDecorators (Proxy :: _ rl)

newtype Decorating f
  = Decorating (forall a. a -> f a)

dk :: forall f. Decorating f -> (forall a. a -> f a)
dk (Decorating f) = f

data Focus a
  = Focus a

class IsAudio (audio :: Type)

instance isAudioAllpass :: IsAudio (CTOR.Allpass a b c)

instance isAudioBandpass :: IsAudio (CTOR.Bandpass a b c)

instance isAudioConstant :: IsAudio (CTOR.Constant a)

instance isAudioConvolver :: IsAudio (CTOR.Convolver a b)

instance isAudioDelay :: IsAudio (CTOR.Delay a b)

instance isAudioDup :: IsAudio (CTOR.Dup a b)

instance isAudioDynamicsCompressor :: IsAudio (CTOR.DynamicsCompressor a b c d e f)

instance isAudioGain :: IsAudio (CTOR.Gain a b)

instance isAudioHighpass :: IsAudio (CTOR.Highpass a b c)

instance isAudioHighshelf :: IsAudio (CTOR.Highshelf a b c)

instance isAudioLoopBuf :: IsAudio (CTOR.LoopBuf a b)

instance isAudioLowpass :: IsAudio (CTOR.Lowpass a b c)

instance isAudioLowshelf :: IsAudio (CTOR.Lowshelf a b c)

instance isAudioMicrophone :: IsAudio CTOR.Microphone

instance isAudioNotch :: IsAudio (CTOR.Notch a b c)

instance isAudioPeaking :: IsAudio (CTOR.Peaking a b c b)

instance isAudioPeriodicOsc :: IsAudio (CTOR.PeriodicOsc a b)

instance isAudioPlayBuf :: IsAudio (CTOR.PlayBuf a b)

instance isAudioRecorder :: IsAudio (CTOR.Recorder a b)

instance isAudioSawtoothOsc :: IsAudio (CTOR.SawtoothOsc a)

instance isAudioSinOsc :: IsAudio (CTOR.SinOsc a)

instance isAudioSpeaker :: IsAudio (CTOR.Speaker a)

instance isAudioSquareOsc :: IsAudio (CTOR.SquareOsc a)

instance isAudioStereoPanner :: IsAudio (CTOR.StereoPanner a b)

instance isAudioTriangleOsc :: IsAudio (CTOR.TriangleOsc a)

instance isAudioWaveShaper :: IsAudio (CTOR.WaveShaper a b c)

instance isAudioProxy :: IsAudio (Proxy s)

instance isAudioIdentity :: IsAudio i => IsAudio (Identity i)

instance isAudioFocus :: IsAudio f => IsAudio (Focus f)

class IsMultiAudio (audio :: Type)

instance isMultiAudioUnit :: IsMultiAudio Unit
else instance isMultiAudioTuple :: (IsMultiAudio a, IsMultiAudio b) => IsMultiAudio (Tuple a b)
else instance isMultiAudioAudio :: IsAudio a => IsMultiAudio a

class IsAudioOrF (audioOrF :: Type)

instance isAudioOrFProxy :: IsAudio a => IsAudioOrF (Proxy s -> a)
else instance isAudioOrFAudio :: IsAudio a => IsAudioOrF a

class IsMultiAudioOrF (audioOrF :: Type)

instance isMultiAudioOrFProxy :: IsMultiAudio a => IsMultiAudioOrF (Proxy s -> a)
else instance isMultiAudioOrFAudio :: IsMultiAudio a => IsMultiAudioOrF a

class IsOversample oversample where
  reflectOversample :: oversample -> Oversample

instance isOversampleNone :: IsOversample CTOR.OversampleNone where
  reflectOversample _ = None 

instance isOversampleTwoX :: IsOversample CTOR.OversampleTwoX where
  reflectOversample _ = TwoX 

instance isOversampleFourX :: IsOversample CTOR.OversampleFourX where
  reflectOversample _ = FourX 