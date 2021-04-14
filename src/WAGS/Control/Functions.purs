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
  , graph
  , lift
  , proof
  , withProof
  , (@>)
  , (@|>)
  ) where

import Prelude
import Control.Monad.State (gets)
import Control.Monad.State as MT
import Data.Either (Either(..))
import Data.Functor.Indexed (imap)
import Data.Map as M
import Data.Tuple.Nested ((/\))
import Type.Data.Peano (Succ)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Change (changes)
import WAGS.Control.MemoizedState (makeMemoizedStateT, runMemoizedStateT')
import WAGS.Control.Qualified as Ix
import WAGS.Control.Thunkable (Thunkable)
import WAGS.Control.Types (AudioState', FrameT, InitialFrameT, SceneT(..), SceneT', oneFrameT, unsafeFrame, unsafeUnframe)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Validation (class GraphIsRenderable, class TerminalIdentityEdge)

startT ::
  forall env audio engine m.
  Monad m =>
  AudioInterpret audio engine =>
  InitialFrameT env audio engine m Unit
startT = unsafeFrame (pure unit)

start :: forall env audio engine. InitialFrameT env audio engine Thunkable Unit
start = unsafeFrame (pure unit)

initialAudioState :: forall env audio engine. env -> AudioState' env audio engine
initialAudioState e =
  { currentIdx: 0
  , env: e
  , instructions: []
  , internalNodes: M.empty
  , internalEdges: M.empty
  }

makeScene ::
  forall env audio engine proofA m i currentIdx graph changeBit skolems a.
  Monad m =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  FrameT env audio engine proofA m i
    (UniverseC currentIdx graph changeBit skolems)
    (Either (SceneT env audio engine proofA m) a) ->
  ( forall proofB.
    FrameT env audio engine proofB m i
      (UniverseC currentIdx graph changeBit skolems)
      a ->
    SceneT env audio engine proofB m
  ) ->
  SceneT env audio engine proofA m
makeScene m trans = SceneT go
  where
  go :: forall proofB. env -> m (SceneT' env audio engine proofB m)
  go ev =
    let
      res =
        runMemoizedStateT'
          (unsafeUnframe m)
          (unsafeCoerce unit)
          (_ { env = ev })
          (initialAudioState ev)
    in
      do
        outcome /\ newState <- res
        case outcome of
          Left s -> oneFrameT s ev
          Right r ->
            pure
              $ { nodes: newState.internalNodes
                , edges: newState.internalEdges
                , instructions: newState.instructions
                , next:
                    ( trans
                        $ unsafeFrame
                            ( makeMemoizedStateT (unsafeCoerce unit)
                                (newState { instructions = [] })
                                r
                            )
                    )
                }

infixr 6 makeScene as @>

branch ::
  forall env audio engine proofA i m currentIdx graph changeBit skolems a.
  Monad m =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  ( forall proofB j.
    FrameT env audio engine proofB m
      (UniverseC currentIdx graph j skolems)
      (UniverseC currentIdx graph j skolems)
      ( Either
          ( FrameT env audio engine proofB m i
              (UniverseC currentIdx graph j skolems)
              a ->
            SceneT env audio engine proofB m
          )
          ( a ->
            FrameT env audio engine proofB m
              (UniverseC currentIdx graph j skolems)
              (UniverseC currentIdx graph (Succ j) skolems)
              a
          )
      )
  ) ->
  FrameT env audio engine proofA m i
    (UniverseC currentIdx graph changeBit skolems)
    a ->
  SceneT env audio engine proofA m
branch mch m =
  makeScene
    ( Ix.do
        r <- m
        mbe <- mch
        case mbe of
          Left l -> changes unit $> Left (l m)
          Right fa -> imap Right (fa r)
    )
    (branch mch)

loop ::
  forall env audio engine proofA i m currentIdx graph changeBit skolems edge a.
  Monad m =>
  AudioInterpret audio engine =>
  TerminalIdentityEdge graph edge =>
  GraphIsRenderable graph =>
  ( forall proofB j.
    a ->
    FrameT env audio engine proofB m (UniverseC currentIdx graph j skolems)
      (UniverseC currentIdx graph (Succ j) skolems)
      a
  ) ->
  FrameT env audio engine proofA m i
    (UniverseC currentIdx graph changeBit skolems)
    a ->
  SceneT env audio engine proofA m
loop fa ma = makeScene (imap Right $ Ix.bind ma fa) (loop fa)

freeze ::
  forall env audio engine proof m i currentIdx graph changeBit skolems x.
  Monad m =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  FrameT env audio engine proof m i
    (UniverseC currentIdx graph changeBit skolems)
    x ->
  SceneT env audio engine proof m
freeze s = makeScene (imap Right s) freeze

makeScene' ::
  forall env audio engine proofA m i currentIdx graph changeBit skolems a.
  Monad m =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  FrameT env audio engine proofA m i
    (UniverseC currentIdx graph changeBit skolems)
    a ->
  ( forall proofB.
    FrameT env audio engine proofB m i
      (UniverseC currentIdx graph changeBit skolems)
      a ->
    SceneT env audio engine proofB m
  ) ->
  SceneT env audio engine proofA m
makeScene' a b = makeScene (imap Right a) b

infixr 6 makeScene' as @|>

env ::
  forall env audio engine proof m i.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m i i env
env = unsafeFrame (gets _.env)

proof ::
  forall env audio engine proof m i.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m i i proof
proof = unsafeFrame (pure (unsafeCoerce unit))

withProof ::
  forall env audio engine proof m i a.
  Monad m =>
  AudioInterpret audio engine =>
  proof -> a -> FrameT env audio engine proof m i i a
withProof _ a = unsafeFrame (pure a)

universe ::
  forall env audio engine proof m i.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m i i (Proxy i)
universe = unsafeFrame $ pure $ (Proxy :: _ i)

graph ::
  forall env audio engine proof m currentIdx graph changeBit skolems.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m
    (UniverseC currentIdx graph changeBit skolems)
    (UniverseC currentIdx graph changeBit skolems)
    (Proxy graph)
graph = unsafeFrame $ pure $ (Proxy :: _ graph)

lift ::
  forall env audio engine proof m i a.
  Monad m =>
  AudioInterpret audio engine =>
  m a -> FrameT env audio engine proof m i i a
lift = unsafeFrame <<< MT.lift
