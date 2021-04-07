module WAGS.Control.Types
  ( Frame(..)
  , AudioState
  , AudioState'
  , InitialUniverse
  , InitialFrame
  , Frame0
  , Scene
  , Scene'
  , oneFrame
  , oneFrame'
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
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Rendered (AnAudioUnit, Instruction)
import WAGS.Universe.Bin (D0)
import Data.Tuple.Nested((/\), type(/\))
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

instance frameFunctor :: Functor (Frame env proof i o) where
  map f (Frame (a)) = Frame (f <$> a)

instance frameIxFunctor :: IxFunctor (Frame env proof) where
  imap f (Frame (a)) = Frame (f <$> a)

instance frameIxApplicative :: IxApply (Frame env proof) where
  iapply (Frame (f)) (Frame (a)) = Frame ((f <*> a))

instance frameIxApply :: IxApplicative (Frame env proof) where
  ipure a = Frame $ (pure a)

instance frameIxBind :: IxBind (Frame env proof) where
  ibind (Frame (monad)) function = Frame ((monad >>= ((\(Frame x) -> x) <<< function)))

instance frameIxMonad :: IxMonad (Frame env proof)

foreign import data Scene :: Type -> Type ->  Type

type role Scene representational representational

type Scene' env proof
  = { nodes :: M.Map Int AnAudioUnit
    , edges :: M.Map Int (Set Int)
    , instructions :: Array Instruction
    , next :: Scene env proof
    }

oneFrame :: forall env proofA. Scene env proofA -> env -> (forall proofB. Scene' env proofB)
oneFrame = unsafeCoerce

oneFrame' :: forall env proofA. Scene env proofA -> env -> (forall proofB. M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array Instruction /\ Scene env proofB)
oneFrame' s e = nodes /\ edges /\ instructions /\ next
  where
  { nodes, edges ,instructions, next } = oneFrame s e