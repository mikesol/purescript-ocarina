module WAGS.Control.Functions
  ( startT
  , start
  , makeScene
  , makeScene'
  , loop
  , branch
  , universe
  , currentIdx
  , changeBit
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

-- | The initial `Frame` that is needed to begin any `Scene`.
-- |
-- | ```purescript
-- | piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
-- | piece =
-- |   Ix.do
-- |     start -- initial frame
-- |     { time } <- env
-- |     create (scene time) $> Right unit
-- |     @> loop
-- |         ( const
-- |             $ Ix.do
-- |                 { time } <- env
-- |                 ivoid $ change (scene time)
-- |         )
-- | ```
start :: forall env audio engine. InitialFrameT env audio engine Thunkable Unit
start = unsafeFrame (pure unit)

-- | The initial `FrameT` that is needed to begin any `SceneT`.
startT ::
  forall env audio engine m.
  Monad m =>
  AudioInterpret audio engine =>
  InitialFrameT env audio engine m Unit
startT = unsafeFrame (pure unit)

initialAudioState :: forall env audio engine. env -> AudioState' env audio engine
initialAudioState e =
  { currentIdx: 0
  , env: e
  , instructions: []
  , internalNodes: M.empty
  , internalEdges: M.empty
  }

-- | Make a scene. The infix operator for this operation is `@>`.
-- |
-- | This function uses the `GraphIsRenderable` typeclass to assert that an audio graph is renderable by the web audio engine. This means, amongst other things, that it has a unique output device (ie speaker), that it does not have any dangling units not connected to a loudspeaker, etc.
-- |
-- | It accepts as arguments:
-- | - a frame to render
-- | - a function that accepts a frame from the next moment in time (`proofB`) and returns a scene.
-- |
-- | From these arguments, it produces a `SceneT`.
-- |
-- | ```purescript
-- | piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
-- | piece =
-- |   Ix.do
-- |     start
-- |     { time } <- env
-- |     create (scene time) $> Right unit
-- |     @> loop -- here, @> is the infix version of `makeScene`
-- |         ( const
-- |             $ Ix.do
-- |                 { time } <- env
-- |                 ivoid $ change (scene time)
-- |         )
-- | ```
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

-- | Loops audio.
-- |
-- | In WAGS, a "loop" is a universe whose `changeBit` increments by 1. That means that the structure of the graph is similar (no units added, none taken away) while some or none of its internal content (ie frequencies, gains, etc) has changed. This is accomplished using the `change` family of functions in `WAGS.Change`.
-- |
-- | ```purescript
-- | piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
-- | piece =
-- |   Ix.do
-- |     start -- initial frame
-- |     { time } <- env
-- |     create (scene time) $> Right unit
-- |     @> loop -- we loop by changing the scene based on `time` in the `env`
-- |         ( const
-- |             $ Ix.do
-- |                 { time } <- env
-- |                 ivoid $ change (scene time)
-- |         )
-- | ```
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

-- | Accepts a "branch" frame for making a scene, where `Left` is a new scene and `Right` is the incoming scene with the change bit incremented by 1. Useful for the common pattern where we loop an audio graph until something in the environment changes, at which point we move on to a new graph.
-- |
-- | ```purescript
-- | simpleScene =
-- |   ( Ix.do
-- |       start
-- |       e <- env
-- |       create (scene0 e) $> Right unit
-- |   )
-- |     @> ( branch \_ -> Ix.do
-- |           { time } <- env
-- |           pr <- proof
-- |           withProof pr
-- |             $ if time < 0.3 then
-- |                 Right
-- |                   (
-- |                       Ix.do
-- |                         e <- env
-- |                         ivoid $ change (scene0 e)
-- |                   )
-- |               else
-- |                 Left
-- |                   ( loop
-- |                       ( const
-- |                           $ Ix.do
-- |                               e <- env
-- |                               ivoid $ change (scene1 e)
-- |                       )
-- |                   )
-- |       )
-- | ```
branch ::
  forall env audio engine proofA i m currentIdx graph changeBit skolems a.
  Monad m =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  ( forall proofB j.
    a ->
    FrameT env audio engine proofB m
      (UniverseC currentIdx graph j skolems)
      (UniverseC currentIdx graph j skolems)
      ( Either
          ( FrameT env audio engine proofB m i
              (UniverseC currentIdx graph j skolems)
              Unit ->
            SceneT env audio engine proofB m
          )
          ( FrameT env audio engine proofB m
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
        mbe <- mch r
        case mbe of
          Left l -> changes unit $> Left (l (m $> unit))
          Right fa -> imap Right fa
    )
    (branch mch)

-- | Freezes the current audio frame.
-- |
-- | ```purescript
-- | scene = (start :*> create (speaker (sinOsc 440.0))) @|> freeze
-- | ```
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

-- | Similar to `makeScene'`, but without the possibility to branch to a new scene. Aliased as `@|>`.
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

-- | Get the environment from a frame.
-- |
-- | ```purescript
-- | piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
-- | piece =
-- |   Ix.do
-- |     start
-- |     { time } <- env -- get the environment
-- |     create (scene time) $> Right unit
-- |     @> loop
-- |         ( const
-- |             $ Ix.do
-- |                 { time } <- env
-- |                 ivoid $ change (scene time)
-- |         )
-- | ```
env ::
  forall env audio engine proof m i.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m i i env
env = unsafeFrame (gets _.env)

-- | Get the proof term from a frame. Useful to construct a new frame using `withProof`.
-- | The following snippet is taken from the WTK example where `proof` is used to generate a proof term that is then consumed by `withProof` in order to make the final `Frame`.
-- |
-- | ```purescript
-- | piece :: { makeRenderingEnv :: MakeRenderingEnv } -> Scene (SceneI Trigger Unit) FFIAudio (Effect Unit) Frame0
-- | piece { makeRenderingEnv } =
-- |   ( Ix.do
-- |       start
-- |       ivoid $ create $ fullKeyboard klavierIdentity
-- |       k0 <- cursor $ cursors.k0
-- |       k1 <- cursor $ cursors.k1
-- |       k2 <- cursor $ cursors.k2
-- |       k3 <- cursor $ cursors.k3
-- |       k4 <- cursor $ cursors.k4
-- |       k5 <- cursor $ cursors.k5
-- |       k6 <- cursor $ cursors.k6
-- |       k7 <- cursor $ cursors.k7
-- |       k8 <- cursor $ cursors.k8
-- |       k9 <- cursor $ cursors.k9
-- |       myProof <- proof
-- |       withProof myProof
-- |         $ Right
-- |             { audioRefs: k0 /\ k1 /\ k2 /\ k3 /\ k4 /\ k5 /\ k6 /\ k7 /\ k8 /\ k9
-- |             , currentKeys: (Nil :: (List KeyInfo))
-- |             , availableKeys: K0 : K1 : K2 : K3 : K4 : K5 : K6 : K7 : K8 : K9 : Nil
-- |             }
-- |   )
-- |     @> loop
-- |         ( \{ audioRefs, currentKeys, availableKeys } -> Ix.do
-- |             { time, trigger, active } <- env
-- |             graphProxy <- graph
-- |             let
-- |               { notesOff
-- |               , onsets
-- |               , newCurrentKeys
-- |               , newAvailableKeys
-- |               , futureCurrentKeys
-- |               , futureAvailableKeys
-- |               } = makeRenderingEnv active trigger time availableKeys currentKeys
-- |             ( playKeys
-- |                 { graphProxy
-- |                 , audioRefs
-- |                 , currentTime: time
-- |                 , notesOff
-- |                 }
-- |                 unit
-- |                 onsets
-- |                 newCurrentKeys
-- |             )
-- |               $> { audioRefs
-- |                 , currentKeys: futureCurrentKeys
-- |                 , availableKeys: futureAvailableKeys
-- |                 }
-- |         )
-- | ```
proof ::
  forall env audio engine proof m i.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m i i proof
proof = unsafeFrame (pure (unsafeCoerce unit))

-- | Consumes a `proof` term to construct a `FrameT`.
-- | This pattern is used because `FrameT` does not implement `IxApplicative`. Instead, in order to construct a frame, one needs to provide `proof` that one is at the current moment in time. This is to prevent frames from different timestamps from mixing.
withProof ::
  forall env audio engine proof m i a.
  Monad m =>
  AudioInterpret audio engine =>
  proof -> a -> FrameT env audio engine proof m i i a
withProof _ a = unsafeFrame (pure a)

-- | Get the current universe as a proxy.
universe ::
  forall env audio engine proof m i.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m i i (Proxy i)
universe = unsafeFrame $ pure $ (Proxy :: _ i)

-- | Get the current index as a proxy.
currentIdx ::
  forall env audio engine proof m currentIdx graph changeBit skolems.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m
    (UniverseC currentIdx graph changeBit skolems)
    (UniverseC currentIdx graph changeBit skolems)
    (Proxy currentIdx)
currentIdx = unsafeFrame $ pure $ (Proxy :: _ currentIdx)

-- | Get the current graph as a proxy.
graph ::
  forall env audio engine proof m currentIdx graph changeBit skolems.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m
    (UniverseC currentIdx graph changeBit skolems)
    (UniverseC currentIdx graph changeBit skolems)
    (Proxy graph)
graph = unsafeFrame $ pure $ (Proxy :: _ graph)

-- | Get the changeBit as a proxy.
changeBit ::
  forall env audio engine proof m currentIdx graph changeBit skolems.
  Monad m =>
  AudioInterpret audio engine =>
  FrameT env audio engine proof m
    (UniverseC currentIdx graph changeBit skolems)
    (UniverseC currentIdx graph changeBit skolems)
    (Proxy changeBit)
changeBit = unsafeFrame $ pure $ (Proxy :: _ changeBit)

-- | Lift a computation from the underlying monad `m` into `FrameT`.
lift ::
  forall env audio engine proof m i a.
  Monad m =>
  AudioInterpret audio engine =>
  m a -> FrameT env audio engine proof m i i a
lift = unsafeFrame <<< MT.lift
