module WAGS.Control.Types
  ( WAG
  , AudioState'
  , InitialGraph
  , InitialFrame
  , InitialWAG
  , Frame0
  , Scene(..)
  , Scene'
  , oneFrame
  , oneFrame'
  , SubScene(..)
  , oneSubFrame
  , oneSubFrame'
  , unsafeUnWAG
  , unsafeWAG
  , class IsScene
  , getFrame
  , unFrame
  ) where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Tuple.Nested ((/\), type (/\))

newtype WAG
  (audio :: Type)
  (engine :: Type)
  (proof :: Type)
  (res :: Type)
  (graph :: Row Type)
  (a :: Type) = WAG { context :: AudioState' audio engine res, value :: a }

derive instance functorWAG :: Functor (WAG audio engine proof res graph)

instance extendWAG :: Extend (WAG audio engine proof res graph) where
  extend f wa@(WAG { context }) = WAG { context, value: f wa }

instance comonadWAG :: Comonad (WAG audio engine proof res graph) where
  extract (WAG { value }) = value

-- | A `proof` term for the initial frame.
data Frame0

-- | The `Graph` at which any scene starts.
type InitialGraph :: forall k. Row k
type InitialGraph = ()

type InitialWAG audio engine res a = WAG audio engine Frame0 res InitialGraph a

-- | The `FrameT` at which any scene starts.
type InitialFrame env audio engine res a =
  env -> WAG audio engine Frame0 res InitialGraph a

-- | Type used for the internal representation of the current audio state.
type AudioState' audio (engine :: Type) res =
  { res :: res
  , instructions :: Array (audio -> engine)
  }

-- | "For office use only" way to access the innards of a frame. Obliterates type safety. Use at your own risk.
unsafeUnWAG
  :: forall audio engine proof res graph a
   . WAG audio engine proof res graph a
  -> { context :: AudioState' audio engine res, value :: a }
unsafeUnWAG (WAG { context, value }) = { context, value }

-- | "For office use only" way to construct a frame. Obliterates type safety. Use at your own risk.
unsafeWAG
  :: forall audio engine proof res graph a
   . { context :: AudioState' audio engine res, value :: a }
  -> WAG audio engine proof res graph a
unsafeWAG = WAG

-- | The information yielded by `oneFrame`.
-- | If `Scene` were a cofree comonad, this would be what is returned by `head` _and_ `tail` combined into one record.
-- | - `instructions`: An array of instructions, ie making things, changing them, or turning them on/off, to be actualized by `audio` and rendered in `engine`.
-- | - `res`: A monoid containing a residual from the audio computation. Use this if you need to pass computations from an audio graph to downstream consumers. In general, it is best if computations happen before audio graph rendering, so it's best to use `res` only in cases where a computation is dependent on values that can only be calculated in the audio-graph, ie scheduling based on the audio clock.
-- | - `next`: The next `Scene`, aka `tail` if `Scene` were a cofree comonad.
type Scene'
  :: forall k
   . (Type -> Type -> Type -> k -> Type -> Type)
  -> Type
  -> Type
  -> Type
  -> k
  -> Type
  -> Type
type Scene' scene env audio engine proof res =
  { instructions :: Array (audio -> engine)
  , res :: res
  , next :: scene env audio engine proof res
  }

-- | An audio scene.
-- |
-- | `Scene` is a sequence of frames that is created using `makeScene`. It is similar to a Cofree Comonad insofar as it is a branching tree that is annotated by the information in a record of type Scene'. However, _unlike_ `Cofree` and unlike comonads in general, the extend/duplicate operation yields a different type on every usage because of the existential `proof` term. Therefore, it cannot implement `Comonad`.  That said, the family of functions starting with `oneFrame` act like `tail` from `Cofree` and are used to peel off a single chunk of rendering information.
newtype Scene :: forall k. Type -> Type -> Type -> k -> Type -> Type
newtype Scene env audio engine proofA res = Scene
  (env -> forall (proofB :: k). Scene' Scene env audio engine proofB res)

class IsScene
  :: forall k. (Type -> Type -> Type -> k -> Type -> Type) -> Constraint
class IsScene scene where
  getFrame
    :: forall env audio engine proofA res
     . scene env audio engine proofA res
    -> (env -> forall proofB. Scene' scene env audio engine proofB res)
  unFrame
    :: forall env audio engine proofA res
     . (env -> forall proofB. Scene' scene env audio engine proofB res)
    -> scene env audio engine proofA res

instance isSceneScene :: IsScene Scene where
  getFrame = oneFrame
  unFrame = Scene

oneFrame
  :: forall env audio engine proofA res
   . Scene env audio engine proofA res
  -> (env -> forall proofB. Scene' Scene env audio engine proofB res)
oneFrame (Scene scene) = scene

-- | This represents the output of `oneFrame` as a tuple instead of a record.
oneFrame'
  :: forall env audio engine proofA res
   . Scene env audio engine proofA res
  -> env
  -> (Array (audio -> engine) /\ res /\ Scene env audio engine proofA res)
oneFrame' s e = go (oneFrame s e)
  where
  go x = instructions /\ res /\ next
    where
    { instructions, res, next } = x

--
newtype SubScene
  :: forall k. Symbol -> Row Type -> Type -> Type -> Type -> k -> Type -> Type
newtype SubScene terminus inputs env audio engine proofA res = SubScene
  ( env
    -> forall (proofB :: k)
     . Scene' (SubScene terminus inputs) env audio engine proofB res
  )

instance isSceneSubScene :: IsScene (SubScene terminus inputs) where
  getFrame = oneSubFrame
  unFrame = SubScene

oneSubFrame
  :: forall terminus inputs env audio engine proofA res
   . SubScene terminus inputs env audio engine proofA res
  -> ( env
       -> forall proofB
        . Scene' (SubScene terminus inputs) env audio engine proofB res
     )
oneSubFrame (SubScene scene) = scene

oneSubFrame'
  :: forall terminus inputs env audio engine proofA res
   . SubScene terminus inputs env audio engine proofA res
  -> env
  -> ( Array (audio -> engine) /\ res /\ SubScene terminus inputs env audio
         engine
         proofA
         res
     )
oneSubFrame' s e = go (oneSubFrame s e)
  where
  go x = instructions /\ res /\ next
    where
    { instructions, res, next } = x

