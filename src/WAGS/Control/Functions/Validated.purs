-- | These functions uses the `GraphIsRenderable` typeclass to assert that an audio graph is renderable by the web audio engine. This means, amongst other things, that it has a unique output device (ie speaker), that it does not have any dangling units not connected to a loudspeaker, etc.
module WAGS.Control.Functions.Validated
  ( makeScene
  , makeScene'
  , loop
  , branch
  , freeze
  , (@>)
  , (@|>)
  ) where

import Prelude
import Data.Either (Either)
import WAGS.Control.Functions as Functions
import WAGS.Control.Types (FrameT, SceneT)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Validation (class GraphIsRenderable)

makeScene ::
  forall env audio engine proofA m res graph a.
  Monad m =>
  Monoid res =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  FrameT env audio engine proofA m res {} { | graph }
    (Either (SceneT env audio engine proofA m res) a) ->
  ( forall proofB.
    FrameT env audio engine proofB m res {} { | graph } a ->
    SceneT env audio engine proofB m res
  ) ->
  SceneT env audio engine proofA m res
makeScene = Functions.makeScene

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
  forall env audio engine proofA m res graph a.
  Monad m =>
  Monoid res =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  ( forall proofB.
    a ->
    FrameT env audio engine proofB m res { | graph }
      { | graph }
      a
  ) ->
  FrameT env audio engine proofA m res {}
    { | graph }
    a ->
  SceneT env audio engine proofA m res
loop = Functions.loop

branch ::
  forall env audio engine proofA m res graph a.
  Monad m =>
  Monoid res =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  ( forall proofB.
    a ->
    FrameT env audio engine proofB m res
      { | graph }
      { | graph }
      ( Either
          ( FrameT env audio engine proofB m res {} { | graph } Unit ->
            SceneT env audio engine proofB m res
          )
          ( FrameT env audio engine proofB m res
              { | graph }
              { | graph }
              a
          )
      )
  ) ->
  FrameT env audio engine proofA m res {} { | graph } a ->
  SceneT env audio engine proofA m res
branch = Functions.branch

freeze ::
  forall env audio engine proof m res graph x.
  Monad m =>
  Monoid res =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  FrameT env audio engine proof m res {} { | graph } x ->
  SceneT env audio engine proof m res
freeze = Functions.freeze

makeScene' ::
  forall env audio engine proofA m res graph a.
  Monad m =>
  Monoid res =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  FrameT env audio engine proofA m res {} { | graph } a ->
  ( forall proofB.
    FrameT env audio engine proofB m res {} { | graph } a ->
    SceneT env audio engine proofB m res
  ) ->
  SceneT env audio engine proofA m res
makeScene' = Functions.makeScene'

infixr 6 makeScene' as @|>
