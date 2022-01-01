-- | These functions uses the `GraphIsRenderable` typeclass to assert that an audio graph is renderable by the web audio engine. This means, amongst other things, that it has a unique output device (ie speaker), that it does not have any dangling units not connected to a loudspeaker, etc.
module WAGS.Control.Functions.Graph
  ( makeScene
  , makeSceneFlipped
  , makeSceneR
  , makeSceneRFlipped
  , makeSceneR'
  , makeSceneR'Flipped
  , startUsing
  , startUsingWithHint
  , loopUsingScene
  , loopUsingSceneWithRes
  , loop
  , iloop
  , branch
  , ibranch
  , istart
  , freeze
  , (@>)
  , (@!>)
  , (@|>)
  , (@||>)
  , (<@)
  , (<|@)
  , (<||@)
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import WAGS.Change (class Change)
import WAGS.Control.Functions (class GraphHint)
import WAGS.Control.Functions as Functions
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene, WAG)
import WAGS.Create (class Create)
import WAGS.CreateT (class CreateT)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Patch (class Patch, PatchInfo)
import WAGS.Validation (class GraphIsRenderable)
import WAGS.WebAPI (BrowserMicrophone)

makeScene
  :: forall env audio engine proofA res graph a
   . Monoid res
  => AudioInterpret audio engine
  => GraphIsRenderable graph
  => (env -> Either (Scene env audio engine proofA res) (WAG audio engine proofA res graph a))
  -> ( forall proofB
        . WAG audio engine proofB res graph a
       -> Scene env audio engine proofB res
     )
  -> Scene env audio engine proofA res
makeScene = Functions.makeScene

infixr 6 makeScene as @>

makeSceneFlipped
  :: forall env audio engine proofA res graph a
   . Monoid res
  => AudioInterpret audio engine
  => GraphIsRenderable graph
  => ( forall proofB
        . WAG audio engine proofB res graph a
       -> Scene env audio engine proofB res
     )
  -> (env -> Either (Scene env audio engine proofA res) (WAG audio engine proofA res graph a))
  -> Scene env audio engine proofA res
makeSceneFlipped = Functions.makeSceneFlipped

infixr 6 makeSceneFlipped as <@

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
loop
  :: forall env audio engine proofA res graph a
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG audio engine proofB res graph a
       -> env
       -> WAG audio engine proofB res graph a
     )
  -> WAG audio engine proofA res graph a
  -> Scene env audio engine proofA res
loop = Functions.loop

iloop
  :: forall env audio engine proofA res graph a
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => ( forall proofB
        . env
       -> a
       -> IxWAG audio engine proofB res graph graph a
     )
  -> WAG audio engine proofA res graph a
  -> Scene env audio engine proofA res
iloop = Functions.iloop

branch
  :: forall env audio engine proofA res graph a
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG audio engine proofB res graph a
       -> env
       -> Either (Scene env audio engine proofB res) (WAG audio engine proofB res graph a)
     )
  -> WAG audio engine proofA res graph a
  -> Scene env audio engine proofA res
branch = Functions.branch

ibranch
  :: forall env audio engine proofA res graph a
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => ( forall proofB
        . env
       -> a
       -> Either
            (WAG audio engine proofB res graph a -> Scene env audio engine proofB res)
            (IxWAG audio engine proofB res graph graph a)
     )
  -> WAG audio engine proofA res graph a
  -> Scene env audio engine proofA res
ibranch = Functions.ibranch

istart
  :: forall env audio engine res graph a
   . GraphIsRenderable graph
  => Monoid res
  => AudioInterpret audio engine
  => (env -> IxWAG audio engine Frame0 res () graph a)
  -> ( forall proofB
        . WAG audio engine proofB res graph a
       -> Scene env audio engine proofB res
     )
  -> Scene env audio engine Frame0 res
istart = Functions.istart

infixr 6 istart as @!>

startUsing
  :: forall env audio engine res graph control
   . Monoid res
  => AudioInterpret audio engine
  => GraphIsRenderable graph
  => Patch () graph
  => PatchInfo
  -> (env -> control)
  -> ( forall proofA
        . WAG audio engine proofA res graph control
       -> Scene env audio engine proofA res
     )
  -> Scene env audio engine Frame0 res
startUsing = Functions.startUsing

startUsingWithHint
  :: forall env audio engine res hintable hint graph control
   . Monoid res
  => AudioInterpret audio engine
  => GraphIsRenderable graph
  => GraphHint hintable hint
  => CreateT hint () graph
  => Patch () graph
  => hintable
  -> PatchInfo
  -> (env -> control)
  -> ( forall proofA
        . WAG audio engine proofA res graph control
       -> Scene env audio engine proofA res
     )
  -> Scene env audio engine Frame0 res
startUsingWithHint = Functions.startUsingWithHint

loopUsingScene
  :: forall env audio engine res scene graph control
   . Monoid res
  => AudioInterpret audio engine
  => Create scene () graph
  => Change scene graph
  => GraphIsRenderable graph
  => (env -> control -> { scene :: { | scene }, control :: control })
  -> (env -> control)
  -> Scene env audio engine Frame0 res
loopUsingScene = Functions.loopUsingScene

loopUsingSceneWithRes
  :: forall env audio engine res scene graph control
   . Monoid res
  => AudioInterpret audio engine
  => Create scene () graph
  => Change scene graph
  => GraphIsRenderable graph
  => (env -> control -> { scene :: { | scene }, control :: control, res :: res })
  -> (env -> control)
  -> Scene env audio engine Frame0 res
loopUsingSceneWithRes = Functions.loopUsingSceneWithRes

freeze
  :: forall env audio engine proof res graph x
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => WAG audio engine proof res graph x
  -> Scene env audio engine proof res
freeze = Functions.freeze

makeSceneR
  :: forall env audio engine proofA res graph a
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => (env -> WAG audio engine proofA res graph a)
  -> ( forall proofB
        . WAG audio engine proofB res graph a
       -> Scene env audio engine proofB res
     )
  -> Scene env audio engine proofA res
makeSceneR = Functions.makeSceneR

infixr 6 makeSceneR as @|>

makeSceneRFlipped
  :: forall env audio engine proofA res graph a
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG audio engine proofB res graph a
       -> Scene env audio engine proofB res
     )
  -> (env -> WAG audio engine proofA res graph a)
  -> Scene env audio engine proofA res
makeSceneRFlipped = Functions.makeSceneRFlipped

infixr 6 makeSceneRFlipped as <|@

makeSceneR'
  :: forall env audio engine proofA res graph a
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => WAG audio engine proofA res graph a
  -> ( forall proofB
        . WAG audio engine proofB res graph a
       -> Scene env audio engine proofB res
     )
  -> Scene env audio engine proofA res
makeSceneR' = Functions.makeSceneR'

infixr 6 makeSceneR' as @||>

makeSceneR'Flipped
  :: forall env audio engine proofA res graph a
   . Monoid res
  => GraphIsRenderable graph
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG audio engine proofB res graph a
       -> Scene env audio engine proofB res
     )
  -> WAG audio engine proofA res graph a
  -> Scene env audio engine proofA res
makeSceneR'Flipped = Functions.makeSceneR'Flipped

infixr 6 makeSceneR'Flipped as <||@
