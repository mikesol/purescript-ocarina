module WAGS.Control.Functions
  ( start
  , modifyRes
  , makeScene
  , makeSceneR
  , makeSceneR'
  , loop
  , iloop
  , branch
  , ibranch
  , iwag
  , freeze
  , (@>)
  , (@|>)
  , (@||>)
  ) where

import Prelude
import Control.Comonad (extract)
import Data.Either (Either(..))
import Data.Map as M
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (AudioState', EFrame, Frame, InitialWAG, Scene(..), Scene', WAG, oneFrame, unsafeUnWAG, unsafeWAG)
import WAGS.Interpret (class AudioInterpret)

-- | The initial `Frame` that is needed to begin any `Scene`.
-- |
-- | ```purescript
-- | piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
-- | piece =
-- |   WAGS.do
-- |     start -- initial frame
-- |     { time } <- env
-- |     create (scene time) $> Right unit
-- |     @> loop
-- |         ( const
-- |             $ WAGS.do
-- |                 { time } <- env
-- |                 ivoid $ change (scene time)
-- |         )
-- | ```
start ::
  forall audio engine res.
  Monoid res =>
  AudioInterpret audio engine =>
  InitialWAG audio engine res Unit
start = unsafeWAG { context: initialAudioState, value: unit }

initialAudioState :: forall audio engine res. Monoid res => AudioState' audio engine res
initialAudioState =
  { res: mempty
  , instructions: []
  , internalNodes: M.empty
  , internalEdges: M.empty
  }

-- | Make a scene. The infix operator for this operation is `@>`.
-- |
-- | It accepts as arguments:
-- | - a frame to render
-- | - a function that accepts a frame from the next moment in time (`proofB`) and returns a scene.
-- |
-- | From these arguments, it produces a `Scene`.
-- |
-- | ```purescript
-- | piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
-- | piece =
-- |   WAGS.do
-- |     start
-- |     { time } <- env
-- |     create (scene time) $> Right unit
-- |     @> loop -- here, @> is the infix version of `makeScene`
-- |         ( const
-- |             $ WAGS.do
-- |                 { time } <- env
-- |                 ivoid $ change (scene time)
-- |         )
-- | ```
makeScene ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  AudioInterpret audio engine =>
  EFrame env audio engine proofA res { | graph } a ->
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    Scene env audio engine proofB res
  ) ->
  Scene env audio engine proofA res
makeScene m trans = Scene go
  where
  go :: forall proofB. env -> Scene' env audio engine proofB res
  go env = case m env of
    Left s -> oneFrame s env
    Right r ->
      let
        { context, value } = unsafeUnWAG r
      in
        { nodes: context.internalNodes
        , edges: context.internalEdges
        , instructions: context.instructions
        , res: context.res
        , next:
            trans
              $ unsafeWAG { context: context { instructions = [], res = mempty }, value }
        }

infixr 6 makeScene as @>

-- | Loops audio.
-- |
-- | In WAGS, a "loop" is a universe whose `changeBit` increments by 1. That means that the structure of the graph is similar (no units added, none taken away) while some or none of its internal content (ie frequencies, gains, etc) has changed. This is accomplished using the `change` family of functions in `WAGS.Change`.
-- |
-- | ```purescript
-- | piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
-- | piece =
-- |   WAGS.do
-- |     start -- initial frame
-- |     { time } <- env
-- |     create (scene time) $> Right unit
-- |     @> loop -- we loop by changing the scene based on `time` in the `env`
-- |         ( const
-- |             $ WAGS.do
-- |                 { time } <- env
-- |                 ivoid $ change (scene time)
-- |         )
-- | ```
loop ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  AudioInterpret audio engine =>
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    Frame env audio engine proofB res { | graph } a
  ) ->
  WAG audio engine proofA res { | graph } a ->
  Scene env audio engine proofA res
loop fa ma = makeSceneR (fa ma) (loop fa)

iloop ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  AudioInterpret audio engine =>
  ( forall proofB.
    env -> a -> IxWAG audio engine proofB res { | graph } { | graph } a
  ) ->
  WAG audio engine proofA res { | graph } a ->
  Scene env audio engine proofA res
iloop fa = loop (\wa e -> let IxWAG f = fa e (extract wa) in f wa)

-- | Accepts a "branch" frame for making a scene, where `Left` is a new scene and `Right` is the incoming scene with the change bit incremented by 1. Useful for the common pattern where we loop an audio graph until something in the environment changes, at which point we move on to a new graph.
-- |
-- | ```purescript
-- | simpleScene =
-- |   ( WAGS.do
-- |       start
-- |       e <- env
-- |       create (scene0 e) $> Right unit
-- |   )
-- |     @> ( branch \_ -> WAGS.do
-- |           { time } <- env
-- |           pr <- proof
-- |           withProof pr
-- |             $ if time < 0.3 then
-- |                 Right
-- |                   (
-- |                       WAGS.do
-- |                         e <- env
-- |                         ivoid $ change (scene0 e)
-- |                   )
-- |               else
-- |                 Left
-- |                   ( loop
-- |                       ( const
-- |                           $ WAGS.do
-- |                               e <- env
-- |                               ivoid $ change (scene1 e)
-- |                       )
-- |                   )
-- |       )
-- | ```
branch ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  AudioInterpret audio engine =>
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    EFrame env audio engine proofB res { | graph } a
  ) ->
  WAG audio engine proofA res { | graph } a ->
  Scene env audio engine proofA res
branch fa w = makeScene (fa w) (branch fa)

iwag ::
  forall env audio engine proof res graph grapho a.
  Monoid res =>
  AudioInterpret audio engine =>
  IxWAG audio engine proof res { | graph } { | grapho } (Scene env audio engine proof res) ->
  WAG audio engine proof res { | graph } a ->
  Scene env audio engine proof res
iwag (IxWAG x) w = extract (x w)

ibranch ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  AudioInterpret audio engine =>
  ( forall proofB.
    env ->
    a ->
    Either
      (WAG audio engine proofB res { | graph } a -> Scene env audio engine proofB res)
      (IxWAG audio engine proofB res { | graph } { | graph } a)
  ) ->
  WAG audio engine proofA res { | graph } a ->
  Scene env audio engine proofA res
ibranch fa =
  branch
    ( \wa e -> case fa e (extract wa) of
        Left l -> Left $ l wa
        Right (IxWAG r) -> Right $ r wa
    )

-- | Freezes the current audio frame.
-- |
-- | ```purescript
-- | scene = (start :*> create (speaker (sinOsc 440.0))) @|> freeze
-- | ```
freeze ::
  forall env audio engine proof res graph x.
  Monoid res =>
  AudioInterpret audio engine =>
  WAG audio engine proof res { | graph } x ->
  Scene env audio engine proof res
freeze s = makeScene (pure $ Right s) freeze

-- | Similar to `makeScene`, but without the possibility to branch to a new scene. Aliased as `@|>`.
makeSceneR ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  AudioInterpret audio engine =>
  Frame env audio engine proofA res { | graph } a ->
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    Scene env audio engine proofB res
  ) ->
  Scene env audio engine proofA res
makeSceneR a b = makeScene (map Right a) b

infixr 6 makeSceneR as @|>

-- | Similar to `makeSceneR'`, but without the possibility to consult an env. Aliased as `@||>`.
makeSceneR' ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  AudioInterpret audio engine =>
  WAG audio engine proofA res { | graph } a ->
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    Scene env audio engine proofB res
  ) ->
  Scene env audio engine proofA res
makeSceneR' a b = makeSceneR (pure a) b

infixr 6 makeSceneR' as @||>

-- | Modifies the residual for a frame and returns the result.
-- | If a frame never modifies its residual, the value of `mempty`
-- | for `res` is returned to the scene.
modifyRes ::
  forall audio engine proof res i a.
  AudioInterpret audio engine =>
  (res -> res) -> WAG audio engine proof res i a -> WAG audio engine proof res i res
modifyRes f w = unsafeWAG { context: (i { res = res' }), value: res' }
  where
  { context: i, value } = unsafeUnWAG w

  res' = f i.res
