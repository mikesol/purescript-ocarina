module WAGS.Control.Types
  ( FrameT
  , Frame
  , AudioState
  , AudioState'
  , InitialUniverse
  , InitialFrameT
  , InitialFrame
  , Frame0
  , SceneT(..)
  , SceneT'
  , Scene
  , Scene'
  , Universe'
  , oneFrame
  , oneFrame'
  , oneFrameT
  , oneFrameT'
  , unsafeUnframe
  , unsafeFrame
  ) where

import Prelude
import Control.Apply.Indexed (class IxApply)
import Data.Functor.Indexed (class IxFunctor)
import Data.Map as M
import Data.Set (Set)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Peano (Z)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Control.MemoizedState (MemoizedStateT)
import WAGS.Control.Thunkable (Thunkable, runThunkable)
import WAGS.Rendered (AnAudioUnit)
import WAGS.Universe.BinN (D0)
import WAGS.Universe.Graph (InitialGraph)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (Universe, UniverseC)

-- | Represents a single frame of an audio scene. Conceptually, this is a snapshot in time of audio.
-- |
-- | - `env`: The outside environment influencing audio. Contains things like mouse clicks, the current time, and MIDI events.
-- | - `audio`: The audio context. This is `Unit` when testing and `FFIAudio` when rendering actual audio.
-- | - `engine`: The type output by the audio rendering engine. This is `Instruction` when testing and `Effect Unit` when rendering actual audio.
-- | - `proof`: A proof term representing the current moment in time. `proof` is a type-safe way to make sure that a frame at time `n` is not composed (ie via bind) with a frame at time `n + 1`.
-- | - `m`: The underlying monad in which the information of the frame lies. Usually this is `Thunkable` but can also be `Identity` or `Aff` depending on your use case.
-- | - `iu`: The input `Universe`, meaning the state of the frame before a computation.
-- | - `ou`: The output `Universe`, meaning the state of the frame after a computation.
-- | - `a`: The term within the frame. This is often some form of accumulator that represents an evolving state over time.
-- |
-- | > NB: `FrameT` does not implement `IxApplicative` because we never want to be able to pull a `proof` term out of thin air. It does, however, have a `bind` operation in `WAGS.Control.Qualified` that can is used for rebindable `do` notation in all of the tests and examples.
newtype FrameT (env :: Type) (audio :: Type) (engine :: Type) (proof :: Type) (m :: Type -> Type) (iu :: Universe) (ou :: Universe) (a :: Type)
  = FrameT (AudioState env audio engine proof m a)


-- | A `FrameT` specialized to the `Thunkable` monad.
type Frame (env :: Type) (audio :: Type) (engine :: Type) (proof :: Type) (iu :: Universe) (ou :: Universe) (a :: Type)
  = FrameT env audio engine proof Thunkable iu ou a

-- | A `proof` term for the initial frame.
data Frame0

-- | The `Universe` at which any scene starts.
type InitialUniverse
  = UniverseC D0 InitialGraph Z SkolemListNil

-- | A type constructor for a graph with no changes and no skolems
type Universe' currentIndex graph cb = UniverseC currentIndex graph cb SkolemListNil

-- | The `FrameT` at which any scene starts.
type InitialFrameT env audio engine m a
  = FrameT env audio engine Frame0 m InitialUniverse InitialUniverse a

-- | The `Frame` at which any scene starts.
type InitialFrame env audio engine a
  = Frame env audio engine Frame0 InitialUniverse InitialUniverse a

instance frameFunctor :: Monad m => Functor (FrameT env audio engine proof m i o) where
  map f (FrameT (a)) = FrameT (f <$> a)

instance frameIxFunctor :: Monad m => IxFunctor (FrameT env audio engine proof m) where
  imap f (FrameT (a)) = FrameT (f <$> a)

instance frameIxApplicative :: Monad m => IxApply (FrameT env audio engine proof m) where
  iapply (FrameT (f)) (FrameT (a)) = FrameT ((f <*> a))

-- | An audio scene.
-- |
-- | `SceneT` is a sequence of frames that is created using `makeScene`. It is similar to a Cofree Comonad insofar as it is a branching tree that is annotated by the information in a record of type SceneT'. However, _unlike_ `Cofree` and unlike comonads in general, the extend/duplicate operation yields a different type on every usage because of the existential `proof` term. Therefore, it cannot implement `Comonad`.  That said, the family of functions starting with `oneFrame` act like `tail` from `Cofree` and are used to peel off a single chunk of rendering information.
data SceneT :: forall k. Type -> Type -> Type -> k -> (Type -> Type) -> Type
data SceneT env audio engine proof m
  = SceneT (env -> m (SceneT' env audio engine proof m))

-- | `Scene` is `SceneT` speacialized to `Thunkable`.
type Scene :: forall k. Type -> Type -> Type -> k -> Type
type Scene env audio engine proof
  = SceneT env audio engine proof Thunkable

-- | The information yielded by `oneFrame`.
-- | If `SceneT` were a cofree comonad, this would be what is returned by `head` _and_ `tail` combined into one record.
-- | - `nodes`: A map of pointers to audio units.
-- | - `edges`: A map of pointers to incoming edges.
-- | - `instructions`: An array of instructions, ie making things, changing them, or turning them on/off, to be actualized by `audio` and rendered in `engine`.
-- | - `next`: The next `SceneT`, aka `tail` if `SceneT` were a cofree comonad.
type SceneT' :: forall k. Type -> Type -> Type -> k -> (Type -> Type) -> Type
type SceneT' env audio engine proof m
  = { nodes :: M.Map Int AnAudioUnit
    , edges :: M.Map Int (Set Int)
    , instructions :: Array (audio -> engine)
    , next :: SceneT env audio engine proof m
    }

-- | `Scene'` is `SceneT'` specialized to `Thunkable`.
type Scene' :: forall k. Type -> Type -> Type -> k -> Type
type Scene' env audio engine proof
  = SceneT' env audio engine proof Thunkable

-- | Given an `env`, gets the next `SceneT'` from a `SceneT`.
oneFrameT :: forall env audio engine proofA m. Monad m => SceneT env audio engine proofA m -> env -> (forall proofB. m (SceneT' env audio engine proofB m))
oneFrameT (SceneT f) = (unsafeCoerce :: (env -> m (SceneT' env audio engine proofA m)) -> (env -> (forall proofB. m (SceneT' env audio engine proofB m)))) f

-- | `oneFrame` is `oneFrameT` specialized for the `Thunkable` monad.
oneFrame :: forall env audio engine proofA. Scene env audio engine proofA -> env -> (forall proofB. Scene' env audio engine proofB)
oneFrame m s = runThunkable (oneFrameT m s)

-- | This represents the output of `oneFrameT` as a tuple instead of a record.
oneFrameT' :: forall env audio engine proofA m. Monad m => SceneT env audio engine proofA m -> env -> (forall proofB. m (M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array (audio -> engine) /\ SceneT env audio engine proofB m))
oneFrameT' s e = go <$> (oneFrameT s e)
  where
  go x = nodes /\ edges /\ instructions /\ next
    where
    { nodes, edges, instructions, next } = x

-- | This represents the output of `oneFrame` as a tuple instead of a record.
oneFrame' :: forall env audio engine proofA. Scene env audio engine proofA -> env -> (forall proofB. (M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array (audio -> engine) /\ Scene env audio engine proofB))
oneFrame' s e = runThunkable (oneFrameT' s e)

-- | Type used for the internal representation of the current audio state.
type AudioState env audio engine proof m a
  = (MemoizedStateT proof (AudioState' env audio engine) m) a

-- | Type used for the internal representation of the current audio state.
type AudioState' env audio (engine :: Type)
  = { env :: env
    , currentIdx :: Int
    , instructions :: Array (audio -> engine)
    , internalNodes :: M.Map Int (AnAudioUnit)
    , internalEdges :: M.Map Int (Set Int)
    }

-- | "For office use only" way to access the innards of a frame. Obliterates type safety. Use at your own risk.
unsafeUnframe :: forall env audio engine proof m iu ou a. FrameT env audio engine proof m iu ou a -> AudioState env audio engine proof m a
unsafeUnframe (FrameT x) = x

-- | "For office use only" way to construct a frame. Obliterates type safety. Use at your own risk.
unsafeFrame :: forall env audio engine proof m iu ou a. AudioState env audio engine proof m a -> FrameT env audio engine proof m iu ou a
unsafeFrame = FrameT