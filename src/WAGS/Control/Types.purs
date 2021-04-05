module WAGS.Control.Types
  ( Frame(..)
  , AudioState
  , AudioState'
  , InitialUniverse
  , InitialFrame
  , Frame0
  , Scene
  , oneFrame
  ) where

import Prelude
import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Functor.Indexed (class IxFunctor)
import Data.Map as M
import Data.Set (Set)
import Data.Tuple.Nested (type (/\))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Rendered (AnAudioUnit, Instruction)
import WAGS.Universe.Bin (D0)
import WAGS.Universe.Graph (InitialGraph)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (Universe, UniverseC)

type AudioState'
  = { currentIdx :: Int
    , instructions :: Array Instruction
    , internalNodes :: M.Map Int (AnAudioUnit)
    , internalEdges :: M.Map Int (Set Int)
    }

type AudioState env a
  = ReaderT env (State (AudioState')) a

newtype Frame (env :: Type) (proof :: Type) (iu :: Universe) (ou :: Universe) (a :: Type)
  = Frame (AudioState env a)

data Frame0

type InitialUniverse
  = UniverseC D0 InitialGraph SkolemListNil

type InitialFrame env a
  = Frame env Frame0 InitialUniverse InitialUniverse a

instance frameIxFunctor :: IxFunctor (Frame env proof) where
  imap f (Frame (a)) = Frame (f <$> a)

instance frameIxApplicative :: IxApply (Frame env proof) where
  iapply (Frame (f)) (Frame (a)) = Frame ((f <*> a))

instance frameIxApply :: IxApplicative (Frame env proof) where
  ipure a = Frame $ (pure a)

instance frameIxBind :: IxBind (Frame env proof) where
  ibind (Frame (monad)) function = Frame ((monad >>= ((\(Frame x) -> x) <<< function)))

instance frameIxMonad :: IxMonad (Frame env proof)

foreign import data Scene :: Type -> Type

type role Scene representational

oneFrame :: forall env. Scene env -> env -> M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array Instruction /\ (Scene env)
oneFrame = unsafeCoerce
