module WAGS.Control.Functions
  ( start
  , makeScene
  , makeScene'
  , loop
  , branch
  , env
  , freeze
  , (@>)
  , (@|>)
  ) where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Bind.Indexed (ibind)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Data.Either (Either(..))
import Data.Functor.Indexed (imap)
import Data.Map as M
import Data.Tuple.Nested ((/\))
import Type.Data.Peano (Succ)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Change (changes)
import WAGS.Control.MemoizedState (makeMemoizedState, runMemoizedState)
import WAGS.Control.Types (AudioState', Frame(..), InitialFrame, Scene, Scene', oneFrame)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Validation (class GraphIsRenderable, class TerminalIdentityEdge)

start :: forall env. InitialFrame env Unit
start = Frame (pure unit)

initialAudioState :: AudioState'
initialAudioState =
  { currentIdx: 0
  , instructions: []
  , internalNodes: M.empty
  , internalEdges: M.empty
  }

asScene :: forall env proof. (env -> Scene' env proof) -> Scene env proof
asScene = unsafeCoerce

makeScene ::
  forall env proofA i currentIdx graph changeBit skolems a.
  GraphIsRenderable graph =>
  Frame env proofA i (UniverseC currentIdx graph changeBit skolems) (Either (Scene env proofA) a) ->
  (forall proofB. Frame env proofB i (UniverseC currentIdx graph changeBit skolems) a -> Scene env proofB) ->
  Scene env proofA
makeScene (Frame m) trans = asScene go
  where
  go ev =
    let
      step1 = runReaderT m ev

      outcome /\ newState = runMemoizedState step1 initialAudioState
    in
      case outcome of
        Left s -> oneFrame s ev
        Right r ->
          { nodes: newState.internalNodes
          , edges: newState.internalEdges
          , instructions: newState.instructions
          , next:
              trans
                $ Frame (ReaderT (pure (makeMemoizedState (newState { instructions = [] }) r)))
          }

infixr 6 makeScene as @>

branch ::
  forall env proofA i currentIdx graph changeBit skolems a.
  GraphIsRenderable graph =>
  (forall proofB j. Frame env proofB (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph j skolems) (Either (Frame env proofB i (UniverseC currentIdx graph j skolems) a -> Scene env proofB) (a -> Frame env proofB (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph (Succ j) skolems) a))) ->
  Frame env proofA i (UniverseC currentIdx graph changeBit skolems) a ->
  Scene env proofA
branch mch m =
  makeScene
    ( Ix.do
        r <- m
        mbe <- mch
        case mbe of
          Left l -> Ix.do
            changes unit
            ipure $ Left (l m)
          Right fa -> imap Right (fa r)
    )
    (branch mch)

loop ::
  forall env proofA i currentIdx graph changeBit skolems edge a.
  TerminalIdentityEdge graph edge =>
  GraphIsRenderable graph =>
  (forall proofB j. a -> Frame env proofB (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph (Succ j) skolems) a) ->
  Frame env proofA i (UniverseC currentIdx graph changeBit skolems) a ->
  Scene env proofA
--loop = branch <<< ipure <<< Right
loop fa ma = makeScene (imap Right $ ibind ma fa) (loop fa)

freeze ::
  forall env proof i currentIdx graph changeBit skolems x.
  GraphIsRenderable graph =>
  Frame env proof i (UniverseC currentIdx graph changeBit skolems) x ->
  Scene env proof
freeze s = makeScene (imap Right s) freeze

makeScene' ::
  forall env proofA i currentIdx graph changeBit skolems a.
  GraphIsRenderable graph =>
  Frame env proofA i (UniverseC currentIdx graph changeBit skolems) a ->
  (forall proofB. Frame env proofB i (UniverseC currentIdx graph changeBit skolems) a -> Scene env proofB) ->
  Scene env proofA
makeScene' a b = makeScene (imap Right a) b

infixr 6 makeScene' as @|>

env ::
  forall env proof i.
  Frame env proof i i env
env = Frame ask
