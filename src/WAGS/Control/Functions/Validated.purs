-- | These functions uses the `GraphIsRenderable` typeclass to assert that an audio graph is renderable by the web audio engine. This means, amongst other things, that it has a unique output device (ie speaker), that it does not have any dangling units not connected to a loudspeaker, etc.
module WAGS.Control.Functions.Validated
  ( makeScene
  , makeSceneR
  , makeSceneR'
  , loop
  , branch
  , freeze
  , (@>)
  , (@|>)
  , (@||>)
  ) where

import Prelude

import WAGS.Control.Functions as Functions
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (EFrame, Frame, Scene, WAG)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Validation (class GraphIsRenderable)

makeScene ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  EFrame env audio engine proofA res { | graph } a ->
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    Scene env audio engine proofB res
  ) ->
  Scene env audio engine proofA res
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
  forall env audio engine proofA res graph a.
  Monoid res =>
  GraphIsRenderable graph =>
  AudioInterpret audio engine =>
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    Frame env audio engine proofB res { | graph } a
  ) ->
  WAG audio engine proofA res { | graph } a ->
  Scene env audio engine proofA res
loop = Functions.loop

iloop ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  GraphIsRenderable graph =>
  AudioInterpret audio engine =>
  ( forall proofB.
    env -> a -> IxWAG audio engine proofB res { | graph } { | graph } a
  ) ->
  WAG audio engine proofA res { | graph } a ->
  Scene env audio engine proofA res
iloop = Functions.iloop

branch ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  GraphIsRenderable graph =>
  AudioInterpret audio engine =>
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    EFrame env audio engine proofB res { | graph } a
  ) ->
  WAG audio engine proofA res { | graph } a ->
  Scene env audio engine proofA res
branch = Functions.branch

freeze ::
  forall env audio engine proof res graph x.
  Monoid res =>
  GraphIsRenderable graph =>
  AudioInterpret audio engine =>
  WAG audio engine proof res { | graph } x ->
  Scene env audio engine proof res
freeze = Functions.freeze

makeSceneR ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  GraphIsRenderable graph =>
  AudioInterpret audio engine =>
  Frame env audio engine proofA res { | graph } a ->
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    Scene env audio engine proofB res
  ) ->
  Scene env audio engine proofA res
makeSceneR = Functions.makeSceneR

infixr 6 makeSceneR as @|>

makeSceneR' ::
  forall env audio engine proofA res graph a.
  Monoid res =>
  GraphIsRenderable graph =>
  AudioInterpret audio engine =>
  WAG audio engine proofA res { | graph } a ->
  ( forall proofB.
    WAG audio engine proofB res { | graph } a ->
    Scene env audio engine proofB res
  ) ->
  Scene env audio engine proofA res
makeSceneR' = Functions.makeSceneR'

infixr 6 makeSceneR' as @||>