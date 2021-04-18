module WAGS.Graph.Decorators where

import Prelude
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Prim.Row (class Cons, class Lacks)
import Prim.RowList as RL
import Record (insert, modify)
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple)
import WAGS.Graph.Constructors as CTOR
import WAGS.Rendered (Oversample(..))

-- A newtype for a decorator, isomorphic to `Exists f` where `f` is a type constructor of type `Type -> Type`. We use decorating instead of `Exists` because `Exists` does not (yet) have an API to work with raw type constructors.
newtype Decorating f
  = Decorating (Decorating' f)

-- | The internal type of decorator.
type Decorating' f
  = forall a. a -> f a

-- | Retrieves the type constructor from decorating, isomorphic to `runExists`.
dk :: forall f. Decorating f -> Decorating' f
dk (Decorating f) = f

-- | A class for the automatic generation of cursors
-- | Given an input row `a` of decorators, outputs a row `b` of `Identity` decorators as well as a Row `c` of `Focus` decorators for each key in `a`.
-- | For example:
-- |
-- | ```purescript
-- | decorators ::
-- |   Tuple
-- |     { d0: Decorating Identity, d1: Decorating Identity }
-- |     { d0: { d0: Decorating Focus, d1: Decorating Identity }
-- |     , d1: { d0: Decorating Identity, d1: Decorating Focus }
-- |     }
-- | decorators = decorate \{d0, d1} ->
-- |   Speaker (dk d0 SinOsc 440.0 /\ dk d1 SinOsc 880.0 /\ unit)
-- | ```
-- |
-- | Check out `test/Ops.purs` for an example of decorators in use.
class Decorate (a :: Row Type) (b :: Row Type) (c :: Row Type) | a -> b c where
  decorate :: forall d. ({ | a } -> d) -> { | b } /\ { | c }

-- Used to focus on a cursor.
data Focus a
  = Focus a

-- Class to determine if a type is audio.
class IsAudio (audio :: Type)

instance isAudioAllpass :: IsAudio (CTOR.Allpass a b c)
else instance isAudioBandpass :: IsAudio (CTOR.Bandpass a b c)
else instance isAudioConstant :: IsAudio (CTOR.Constant a)
else instance isAudioConvolver :: IsAudio (CTOR.Convolver a b)
else instance isAudioDelay :: IsAudio (CTOR.Delay a b)
else instance isAudioDup :: IsAudio (CTOR.Dup a b)
else instance isAudioDynamicsCompressor :: IsAudio (CTOR.DynamicsCompressor a b c d e f)
else instance isAudioGain :: IsAudio (CTOR.Gain a b)
else instance isAudioHighpass :: IsAudio (CTOR.Highpass a b c)
else instance isAudioHighshelf :: IsAudio (CTOR.Highshelf a b c)
else instance isAudioLoopBuf :: IsAudio (CTOR.LoopBuf a b)
else instance isAudioLowpass :: IsAudio (CTOR.Lowpass a b c)
else instance isAudioLowshelf :: IsAudio (CTOR.Lowshelf a b c)
else instance isAudioMicrophone :: IsAudio CTOR.Microphone
else instance isAudioNotch :: IsAudio (CTOR.Notch a b c)
else instance isAudioPeaking :: IsAudio (CTOR.Peaking a b c b)
else instance isAudioPeriodicOsc :: IsAudio (CTOR.PeriodicOsc a b)
else instance isAudioPlayBuf :: IsAudio (CTOR.PlayBuf a b)
else instance isAudioRecorder :: IsAudio (CTOR.Recorder a b)
else instance isAudioSawtoothOsc :: IsAudio (CTOR.SawtoothOsc a)
else instance isAudioSinOsc :: IsAudio (CTOR.SinOsc a)
else instance isAudioSpeaker :: IsAudio (CTOR.Speaker a)
else instance isAudioSquareOsc :: IsAudio (CTOR.SquareOsc a)
else instance isAudioStereoPanner :: IsAudio (CTOR.StereoPanner a b)
else instance isAudioTriangleOsc :: IsAudio (CTOR.TriangleOsc a)
else instance isAudioWaveShaper :: IsAudio (CTOR.WaveShaper a b c)
else instance isAudioProxy :: IsAudio (Proxy s)
else instance isAudioFofAudio :: IsAudio i => IsAudio (f i)

-- | Class to determine if a type is audio or multiple audios.
class IsMultiAudio (audio :: Type)

instance isMultiAudioUnit :: IsMultiAudio Unit
else instance isMultiAudioTuple :: (IsMultiAudio a, IsMultiAudio b) => IsMultiAudio (Tuple a b)
else instance isMultiAudioAudio :: IsAudio a => IsMultiAudio a

-- | Class to determine if a type is audio or a closure producing audio.
class IsAudioOrF (audioOrF :: Type)

instance isAudioOrFProxy :: IsAudio a => IsAudioOrF (Proxy s -> a)
else instance isAudioOrFAudio :: IsAudio a => IsAudioOrF a

-- | Class to determine if a type is audio, multiple audios, or a closure producing audio or multiple audios.
class IsMultiAudioOrF (audioOrF :: Type)

instance isMultiAudioOrFProxy :: IsMultiAudio a => IsMultiAudioOrF (Proxy s -> a)
else instance isMultiAudioOrFAudio :: IsMultiAudio a => IsMultiAudioOrF a

-- | Class to determine if a type is an oversample directive.
class IsOversample oversample where
  reflectOversample :: oversample -> Oversample

instance isOversampleNone :: IsOversample CTOR.OversampleNone where
  reflectOversample _ = None

instance isOversampleTwoX :: IsOversample CTOR.OversampleTwoX where
  reflectOversample _ = TwoX

instance isOversampleFourX :: IsOversample CTOR.OversampleFourX where
  reflectOversample _ = FourX

-- | Internal helper class to construct decorators.
class MakeDecorators (rl :: RL.RowList Type) b | rl -> b where
  makeDecorators :: Proxy rl -> { | b }

instance makeDecoratorsRLCons :: (IsSymbol sym, Lacks sym rest, Cons sym (Decorating Identity) rest r, MakeDecorators b rest) => MakeDecorators (RL.Cons sym (Decorating f) b) r where
  makeDecorators _ = insert (Proxy :: _ sym) (Decorating Identity) (makeDecorators (Proxy :: _ b))

instance makeDecoratorsRLNil :: MakeDecorators RL.Nil () where
  makeDecorators _ = {}

-- | Internal helper class to construct focusing decorators.
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

