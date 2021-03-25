module Stream7
  ( class CSinOsc
  , SinOsc
  , sinOsc
  , class CHighpass
  , Highpass
  , highpass
  , SceneT
  , class AudioUnit
  , audioInfo
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Cont (class MonadCont)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadTrans, StateT, gets, modify_)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus, class MonadZero)
import Control.Plus (class Plus)
import Data.Identity (Identity)
import Data.Map (Map, insert)
import Data.Typelevel.Num (D0, d0)
import Effect.Class (class MonadEffect)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)


-----------------------------------------
----------------------
----- problem with this sketch is that
----- the pointers to the graph, ie an individual sinosc
----- do not have enough type-level information to relate to the graph
----- they can be this poor at the _term_ level, but _not_ at the type level!

newtype SceneT m a
  = Scene (StateT { currentIdx :: Int, graph :: Map Int AudioUnit_ } m a)

derive newtype instance functorScene :: Functor m => Functor (SceneT m)

derive newtype instance applyScene :: Monad m => Apply (SceneT m)

derive newtype instance applicativeScene :: Monad m => Applicative (SceneT m)

derive newtype instance bindScene :: Monad m => Bind (SceneT m)

derive newtype instance monadScene :: Monad m => Monad (SceneT m)

derive newtype instance monadTransScene :: MonadTrans SceneT

derive newtype instance altScene :: (Monad m, Alt m) => Alt (SceneT m)

derive newtype instance plusScene :: (Monad m, Plus m) => Plus (SceneT m)

derive newtype instance alternativeScene :: (Monad m, Alternative m) => Alternative (SceneT m)

derive newtype instance monadRecScene :: (Monad m, MonadRec m) => MonadRec (SceneT m)

derive newtype instance monadZeroScene :: (Monad m, MonadZero m) => MonadZero (SceneT m)

derive newtype instance monadPlusScene :: (Monad m, MonadPlus m) => MonadPlus (SceneT m)

derive newtype instance lazyScene :: Lazy (SceneT m a)

derive newtype instance monadEffectScene :: (Monad m, MonadEffect m) => MonadEffect (SceneT m)

derive newtype instance monadContScene :: (Monad m, MonadCont m) => MonadCont (SceneT m)

derive newtype instance monadThrowScene :: (Monad m, MonadThrow e m) => MonadThrow e (SceneT m)

derive newtype instance monadErrorScene :: (Monad m, MonadError e m) => MonadError e (SceneT m)

derive newtype instance monadAskScene :: (Monad m, MonadAsk r m) => MonadAsk r (SceneT m)

derive newtype instance monadReaderScene :: (Monad m, MonadReader r m) => MonadReader r (SceneT m)

derive newtype instance monadTellScene :: (Monad m, MonadTell w m) => MonadTell w (SceneT m)

derive newtype instance monadWriterScene :: (Monad m, MonadWriter w m) => MonadWriter w (SceneT m)

derive newtype instance semigroupScene :: (Monad m, Semigroup a) => Semigroup (SceneT m a)

derive newtype instance monoidScene :: (Monad m, Monoid a) => Monoid (SceneT m a)

defaultParam :: AudioParameter'
defaultParam = { param: 0.0, timeOffset: 0.0, transition: LinearRamp, forceSet: false }

data AudioParameterTransition
  = NoRamp
  | LinearRamp
  | ExponentialRamp
  | Immediately

type AudioParameter'
  = { param :: Number
    , timeOffset :: Number
    , transition :: AudioParameterTransition
    , forceSet :: Boolean
    }

newtype AudioParameter
  = AudioParameter AudioParameter'

type Scene
  = SceneT Identity

newtype SinOsc
  = SinOsc Int

newtype Highpass (nChans :: Type)
  = Highpass { nChans :: nChans, idx :: Int }

data AudioUnit_
  = SinOsc_ { idx :: Int, freq :: AudioParameter }
  | Highpass_ { idx :: Int, freq :: AudioParameter, q :: AudioParameter, a :: Int }

simpleBulder :: forall a. (Int -> a) -> (Int -> AudioUnit_) -> (Scene a)
simpleBulder ia iau =
  Scene
    $ do
        idx <- gets _.currentIdx
        modify_ (\i -> i { currentIdx = idx + 1, graph = insert idx (iau idx) i.graph })
        pure $ ia idx

type AudioInfo ch
  = { nChans :: ch, idx :: Int }

class AudioUnit a ch | a -> ch where
  audioInfo :: a -> AudioInfo ch

class Modify k v x g where
  modify :: k -> (g -> x -> v -> v) -> Scene Unit

newtype Freq = Freq Number

newtype Q = Q Number

class CSinOsc freq where
  sinOsc :: { freq :: freq } -> Scene SinOsc

instance cSinOscNumber :: CSinOsc Number where
  sinOsc { freq } = simpleBulder SinOsc (\idx -> SinOsc_ { freq: AudioParameter $ defaultParam { param = freq }, idx })

instance cSinOscAudioParameter :: CSinOsc AudioParameter where
  sinOsc { freq } = simpleBulder SinOsc (\idx -> SinOsc_ { freq, idx })

instance audioUnitSinOsc :: AudioUnit SinOsc D0 where
  audioInfo a = { nChans: d0, idx: coerce a }

class CHighpass freq q where
  highpass :: forall a ch. AudioUnit a ch => { freq :: freq, q :: q, a :: a } -> Scene (Highpass ch)

instance cHighPassFreqNumberQNumber :: CHighpass Number Number where
  highpass { freq, q, a } = highpass { freq: AudioParameter $ defaultParam { param = freq }, q: AudioParameter $ defaultParam { param = q }, a }

instance cHighPassFreqNumberQAudioParameter :: CHighpass Number AudioParameter where
  highpass { freq, q, a } = highpass { freq: AudioParameter $ defaultParam { param = freq }, q, a }

instance cHighPassFreqAudioParameterQNumber :: CHighpass AudioParameter Number where
  highpass { freq, q, a } = highpass { freq, q: AudioParameter $ defaultParam { param = q }, a }

instance cHighPassFreqAudioParamQAudioParam :: CHighpass AudioParameter AudioParameter where
  highpass { freq, q, a } = let ai = audioInfo a in simpleBulder (\idx -> Highpass { idx, nChans: ai.nChans }) (\idx -> Highpass_ { freq, q, a: ai.idx, idx })

instance audioUnitHighPass :: AudioUnit a ch => AudioUnit (Highpass ch) ch where
  audioInfo (Highpass a) = { idx: a.idx, nChans: unsafeCoerce a.nChans }
{-
do
  so <- sinOsc {freq: 440.0}
  hp <  highpass {freq: 440.0, q: 1.0 } so
  -- here, the modifcation will persist everywhere
  -- so really, sinOscs don't need to hold their values at all
  -- nothing does
  -- it can all be done in the monad
  -- the only thing they need to hold is their index in the monad so that
  -- they can update the object
  modify so 440.0
-}
