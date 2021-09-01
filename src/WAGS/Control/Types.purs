module WAGS.Control.Types
  ( Frame
  , EFrame
  , WAG
  , AudioState'
  , InitialGraph
  , InitialFrame
  , InitialWAG
  , Frame0
  , Scene(..)
  , Scene'
  , oneFrame
  , oneFrame'
  , unsafeUnWAG
  , unsafeWAG
  ) where

import Prelude
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Either (Either)
import Data.Tuple.Nested ((/\), type (/\))

newtype WAG (assets :: Row Type) (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type) (graph :: Type) (a :: Type)
  = WAG { context :: AudioState' audio engine res, value :: a }

derive instance functorWAG :: Functor (WAG assets audio engine proof res graph)

instance extendWAG :: Extend (WAG assets audio engine proof res graph) where
  extend f wa@(WAG { context }) = WAG { context, value: f wa }

instance comonadWAG :: Comonad (WAG assets audio engine proof res graph) where
  extract (WAG { value }) = value

-- | Represents a single frame of an audio scene. Conceptually, this is a snapshot in time of audio.
-- |
-- | - `env`: The outside environment influencing audio. Contains things like mouse clicks, the current time, and MIDI events.
-- | - `audio`: The audio context. This is `Unit` when testing and `FFIAudio` when rendering actual audio.
-- | - `engine`: The type output by the audio rendering engine. This is `Instruction` when testing and `Effect Unit` when rendering actual audio.
-- | - `proof`: A proof term representing the current moment in time. `proof` is a type-safe way to make sure that a frame at time `n` is not composed (ie via bind) with a frame at time `n + 1`.
-- | - `res`: A monoid containing a residual from the audio computation. Use this if you need to pass computations from an audio graph to downstream consumers. In general, it is best if computations happen before audio graph rendering, so it's best to use `res` only in cases where a computation is dependent on values that can only be calculated in the audio-graph, ie scheduling based on the audio clock.
-- | - `graph`: The `Graph`, meaning the current audio graph of the frame.
-- | - `a`: The term within the frame. This is often some form of accumulator that represents an evolving state over time.
-- |
type Frame (env :: Type) (assets :: Row Type) (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type) (graph :: Type) (a :: Type)
  = env -> WAG assets audio engine proof res graph a

type EFrame (env :: Type) (assets :: Row Type) (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type) (graph :: Type) (a :: Type)
  = env -> Either (Scene env assets audio engine proof res) (WAG assets audio engine proof res graph a)

-- | A `proof` term for the initial frame.
data Frame0

-- | The `Graph` at which any scene starts.
type InitialGraph
  = {}

type InitialWAG assets audio engine res a
  = WAG assets audio engine Frame0 res InitialGraph a

-- | The `FrameT` at which any scene starts.
type InitialFrame assets env audio engine res a
  = Frame assets env audio engine Frame0 res InitialGraph a

-- | An audio scene.
-- |
-- | `Scene` is a sequence of frames that is created using `makeScene`. It is similar to a Cofree Comonad insofar as it is a branching tree that is annotated by the information in a record of type Scene'. However, _unlike_ `Cofree` and unlike comonads in general, the extend/duplicate operation yields a different type on every usage because of the existential `proof` term. Therefore, it cannot implement `Comonad`.  That said, the family of functions starting with `oneFrame` act like `tail` from `Cofree` and are used to peel off a single chunk of rendering information.
newtype Scene :: forall k. Type -> Row Type -> Type -> Type -> k -> Type -> Type
newtype Scene env assets audio engine proofA res
  = Scene (env -> forall (proofB :: k). Scene' env assets audio engine proofB res)

-- | The information yielded by `oneFrame`.
-- | If `Scene` were a cofree comonad, this would be what is returned by `head` _and_ `tail` combined into one record.
-- | - `instructions`: An array of instructions, ie making things, changing them, or turning them on/off, to be actualized by `audio` and rendered in `engine`.
-- | - `res`: A monoid containing a residual from the audio computation. Use this if you need to pass computations from an audio graph to downstream consumers. In general, it is best if computations happen before audio graph rendering, so it's best to use `res` only in cases where a computation is dependent on values that can only be calculated in the audio-graph, ie scheduling based on the audio clock.
-- | - `next`: The next `Scene`, aka `tail` if `Scene` were a cofree comonad.
type Scene' :: forall k. Type -> Row Type -> Type -> Type -> k -> Type -> Type
type Scene' env assets audio engine proof res
  =
  { instructions :: Array (audio -> engine)
  , res :: res
  , next :: Scene env assets audio engine proof res
  }

oneFrame :: forall env assets audio engine proofA res. Scene env assets audio engine proofA res -> (env -> forall proofB. Scene' env assets audio engine proofB res)
oneFrame (Scene scene) = scene

-- | This represents the output of `oneFrame` as a tuple instead of a record.
oneFrame' :: forall env assets audio engine proofA res. Scene env assets audio engine proofA res -> env -> (Array (audio -> engine) /\ res /\ Scene env assets audio engine proofA res)
oneFrame' s e = go (oneFrame s e)
  where
  go x = instructions /\ res /\ next
    where
    { instructions, res, next } = x

-- | Type used for the internal representation of the current audio state.
type AudioState' audio (engine :: Type) res
  =
  { res :: res
  , instructions :: Array (audio -> engine)
  }

-- | "For office use only" way to access the innards of a frame. Obliterates type safety. Use at your own risk.
unsafeUnWAG
  :: forall assets audio engine proof res graph a
   . WAG assets audio engine proof res graph a
  -> { context :: AudioState' audio engine res, value :: a }
unsafeUnWAG (WAG { context, value }) = { context, value }

-- | "For office use only" way to construct a frame. Obliterates type safety. Use at your own risk.
unsafeWAG
  :: forall assets audio engine proof res graph a
   . { context :: AudioState' audio engine res, value :: a }
  -> WAG assets audio engine proof res graph a
unsafeWAG = WAG
