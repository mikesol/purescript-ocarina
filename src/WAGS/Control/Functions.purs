module WAGS.Control.Functions
  ( startT
  , start
  , makeScene
  , makeScene'
  , loop
  , branch
  , universe
  , env
  , freeze
  , (@>)
  , (@|>)
  ) where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Bind.Indexed (ibind)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.State (gets)
import Data.Either (Either(..))
import Data.Functor.Indexed (imap)
import Data.Identity (Identity)
import Data.Map as M
import Data.Tuple.Nested ((/\))
import Type.Data.Peano (Succ)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Change (changes)
import WAGS.Control.MemoizedState (makeMemoizedStateT, runMemoizedStateT')
import WAGS.Control.Types (AudioState', FrameT(..), InitialFrameT, SceneT(..), SceneT', oneFrameT)
import WAGS.Rendered (sortInstructions)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Validation (class GraphIsRenderable, class TerminalIdentityEdge)

startT :: forall env m. Monad m => InitialFrameT env m Unit
startT = FrameT (pure unit)

start :: forall env. InitialFrameT env Identity Unit
start = FrameT (pure unit)

initialAudioState :: forall env. env -> AudioState' env
initialAudioState e =
  { currentIdx: 0
  , env: e
  , instructions: []
  , internalNodes: M.empty
  , internalEdges: M.empty
  }

makeScene ::
  forall env proofA m i currentIdx graph changeBit skolems a.
  Monad m =>
  GraphIsRenderable graph =>
  FrameT env proofA m i (UniverseC currentIdx graph changeBit skolems) (Either (SceneT env proofA m) a) ->
  (forall proofB. FrameT env proofB m i (UniverseC currentIdx graph changeBit skolems) a -> SceneT env proofB m) ->
  SceneT env proofA m
makeScene (FrameT m) trans = SceneT go
  where
  go :: forall proofB. env -> m (SceneT' env proofB m)
  go ev =
    let
      res = runMemoizedStateT' m (unsafeCoerce unit) (_ { env = ev }) (initialAudioState ev)
    in
      do
        outcome /\ newState <- res
        case outcome of
          Left s -> oneFrameT s ev
          Right r ->
            pure
              $ { nodes: newState.internalNodes
                , edges: newState.internalEdges
                , instructions: (sortInstructions newState.instructions)
                , next:
                    ( trans
                        $ FrameT
                            ( makeMemoizedStateT (unsafeCoerce unit)
                                (newState { instructions = [] })
                                r
                            )
                    )
                }

infixr 6 makeScene as @>

branch ::
  forall env proofA i m currentIdx graph changeBit skolems a.
  Monad m =>
  GraphIsRenderable graph =>
  (forall proofB j. FrameT env proofB m (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph j skolems) (Either (FrameT env proofB m i (UniverseC currentIdx graph j skolems) a -> SceneT env proofB m) (a -> FrameT env proofB m (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph (Succ j) skolems) a))) ->
  FrameT env proofA m i (UniverseC currentIdx graph changeBit skolems) a ->
  SceneT env proofA m
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
  forall env proofA i m currentIdx graph changeBit skolems edge a.
  Monad m =>
  TerminalIdentityEdge graph edge =>
  GraphIsRenderable graph =>
  (forall proofB j. a -> FrameT env proofB m (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph (Succ j) skolems) a) ->
  FrameT env proofA m i (UniverseC currentIdx graph changeBit skolems) a ->
  SceneT env proofA m
--loop = branch <<< ipure <<< Right
loop fa ma = makeScene (imap Right $ ibind ma fa) (loop fa)

freeze ::
  forall env proof m i currentIdx graph changeBit skolems x.
  Monad m =>
  GraphIsRenderable graph =>
  FrameT env proof m i (UniverseC currentIdx graph changeBit skolems) x ->
  SceneT env proof m
freeze s = makeScene (imap Right s) freeze

makeScene' ::
  forall env proofA m i currentIdx graph changeBit skolems a.
  Monad m =>
  GraphIsRenderable graph =>
  FrameT env proofA m i (UniverseC currentIdx graph changeBit skolems) a ->
  (forall proofB. FrameT env proofB m i (UniverseC currentIdx graph changeBit skolems) a -> SceneT env proofB m) ->
  SceneT env proofA m
makeScene' a b = makeScene (imap Right a) b

infixr 6 makeScene' as @|>

env ::
  forall env proof m i.
  Monad m =>
  FrameT env proof m i i env
env = FrameT (gets _.env)

universe ::
  forall env proof m i o.
  Monad m =>
  FrameT env proof m i o (Proxy o)
universe = FrameT $ pure $ (Proxy :: _ o)
