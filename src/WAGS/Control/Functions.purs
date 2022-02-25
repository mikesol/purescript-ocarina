module WAGS.Control.Functions
  ( start
  , istart
  , startUsing
  , startUsingWithHint
  , loopUsingScene
  , loopUsingSceneWithRes
  , modifyRes
  , imodifyRes
  , makeScene
  , makeSceneFlipped
  , makeSceneR
  , makeSceneRFlipped
  , makeSceneR'
  , makeSceneR'Flipped
  , loop
  , iloop
  , branch
  , ibranch
  , icont
  , freeze
  , (@>)
  , (@!>)
  , (@|>)
  , (@||>)
  , (<@)
  , (<|@)
  , (<||@)
  , class GraphHint
  ) where

import Prelude

import Control.Apply.Indexed ((:*>))
import Control.Comonad (extract)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Record as R
import WAGS.Change (class Change, ichange)
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (class IsScene, AudioState', Frame0, InitialWAG, Scene', WAG, getFrame, unFrame, unsafeUnWAG, unsafeWAG)
import WAGS.Create (class Create, icreate)
import WAGS.CreateT (class CreateT)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Patch (class Patch, PatchInfo, ipatch)
import WAGS.WebAPI (BrowserMicrophone)

-- | The initial `Frame` that is needed to begin any `Scene`.
-- |
-- | ```purescript
-- | piece :: Scene (BehavingScene Unit Unit) FFIAudio (Effect Unit) Frame0
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
start
  :: forall audio engine res
   . Monoid res
  => AudioInterpret audio engine
  => InitialWAG audio engine res Unit
start = unsafeWAG { context: initialAudioState, value: unit }

initialAudioState :: forall audio engine res. Monoid res => AudioState' audio engine res
initialAudioState =
  { res: mempty
  , instructions: []
  }

-- | Make a scene. The infix operator for this operation is `@>`.
-- |
-- | It accepts as arguments:
-- | - a frame to render
-- | - a function that accepts a frame from the next moment in time (`proofB`) and returns a scene.
-- |
-- | From these arguments, it produces a `Scene`.
-- | ```
makeScene
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => IsScene scene
  => AudioInterpret audio engine
  => (env -> Either (scene env audio engine proofA res) (WAG audio engine proofA res graph control))
  -> ( forall proofB
        . WAG audio engine proofB res graph control
       -> scene env audio engine proofB res
     )
  -> scene env audio engine proofA res
makeScene m trans = unFrame go
  where
  go :: forall proofB. env -> Scene' scene env audio engine proofB res
  go env = case m env of
    Left s -> getFrame s env
    Right r ->
      let
        { context, value } = unsafeUnWAG r
      in
        { instructions: context.instructions
        , res: context.res
        , next:
            trans
              $ unsafeWAG { context: context { instructions = [], res = mempty }, value }
        }

infixr 6 makeScene as @>

makeSceneFlipped
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => ( forall proofB
        . WAG audio engine proofB res graph control
       -> scene env audio engine proofB res
     )
  -> (env -> Either (scene env audio engine proofA res) (WAG audio engine proofA res graph control))
  -> scene env audio engine proofA res
makeSceneFlipped trans m = makeScene m trans

infixr 6 makeSceneFlipped as <@

istart
  :: forall scene env audio engine res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => (env -> IxWAG audio engine Frame0 res () graph control)
  -> ( forall proofB
        . WAG audio engine proofB res graph control
       -> scene env audio engine proofB res
     )
  -> scene env audio engine Frame0 res
istart m = makeSceneR (\e -> let IxWAG f = m e in f start)

infixr 6 istart as @!>

startUsing
  :: forall scene env audio engine res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => Patch () graph
  => PatchInfo
  -> (env -> control)
  -> ( forall proofA
        . WAG audio engine proofA res graph control
       -> scene env audio engine proofA res
     )
  -> scene env audio engine Frame0 res
startUsing patchInfo control next = (\e -> (ipatch patchInfo $> control e)) @!> next

class GraphHint (i :: Type) (o :: Row Type) | i -> o

instance graphHintRec :: GraphHint { | o } o

instance graphHintTuple :: GraphHint right o => GraphHint (left /\ right) o

instance graphHintF :: GraphHint x o => GraphHint (y -> x) o

startUsingWithHint
  :: forall scene env audio engine res hintable hint graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => GraphHint hintable hint
  => CreateT hint () graph
  => Patch () graph
  => hintable
  -> PatchInfo
  -> (env -> control)
  -> ( forall proofA
        . WAG audio engine proofA res graph control
       -> scene env audio engine proofA res
     )
  -> scene env audio engine Frame0 res
startUsingWithHint _ patchInfo control next = (\e -> (ipatch patchInfo $> control e)) @!> next

loopUsingScene
  :: forall scene env audio engine res sn graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => Create sn () graph
  => Change sn graph
  => (env -> control -> { scene :: { | sn }, control :: control })
  -> (env -> control)
  -> scene env audio engine Frame0 res
loopUsingScene = loopUsingSceneWithRes <<< f
  where
  f = (map <<< map) (R.union { res: mempty :: res })

loopUsingSceneWithRes
  :: forall scene env audio engine res sn graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => Create sn () graph
  => Change sn graph
  => (env -> control -> { scene :: { | sn }, control :: control, res :: res })
  -> (env -> control)
  -> scene env audio engine Frame0 res
loopUsingSceneWithRes sceneF initialControl =
  (\env -> let { scene, control, res } = sceneF env (initialControl env) in icreate scene :*> imodifyRes (const res) $> control) @!>
    iloop \env icontrol ->
      let { scene, control, res } = sceneF env icontrol in ichange scene *> imodifyRes (const res) $> control

-- | Loops audio.
-- |
-- | The first argument is the loop and the second argument is the incoming graph that gets rendered before the loop.
-- | This means that all changes applied in the loop must be separately applied to the incoming frame if they are relevant.
-- | ```
loop
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => ( forall proofB
        . WAG audio engine proofB res graph control
       -> env
       -> WAG audio engine proofB res graph control
     )
  -> WAG audio engine proofA res graph control
  -> scene env audio engine proofA res
loop fa ma = makeSceneR (fa ma) (loop fa)

iloop
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => ( forall proofB
        . env
       -> control
       -> IxWAG audio engine proofB res graph graph control
     )
  -> WAG audio engine proofA res graph control
  -> scene env audio engine proofA res
iloop fa = loop (\wa e -> let IxWAG f = fa e (extract wa) in f wa)

-- | Accepts a "branch" frame for making a scene, where `Left` is a new scene and `Right` is the current scene looped.
-- |
branch
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => ( forall proofB
        . WAG audio engine proofB res graph control
       -> env
       -> Either (scene env audio engine proofB res) (WAG audio engine proofB res graph control)
     )
  -> WAG audio engine proofA res graph control
  -> scene env audio engine proofA res
branch fa w = makeScene (fa w) (branch fa)

icont
  :: forall scene env audio engine proof res graphi grapho a b
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => ( WAG audio engine proof res grapho b
       -> scene env audio engine proof res
     )
  -> IxWAG audio engine proof res graphi grapho b
  -> WAG audio engine proof res graphi a
  -> scene env audio engine proof res
icont c (IxWAG x) = c <<< x

ibranch
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => ( forall proofB
        . env
       -> control
       -> Either
            (WAG audio engine proofB res graph control -> scene env audio engine proofB res)
            (IxWAG audio engine proofB res graph graph control)
     )
  -> WAG audio engine proofA res graph control
  -> scene env audio engine proofA res
ibranch fa =
  branch
    ( \wa e -> case fa e (extract wa) of
        Left l -> Left $ l wa
        Right (IxWAG r) -> Right $ r wa
    )

-- | Freezes the current audio frame.
-- |
freeze
  :: forall scene env audio engine proof res graph x
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => WAG audio engine proof res graph x
  -> scene env audio engine proof res
freeze s = makeScene (pure $ Right s) freeze

-- | Similar to `makeScene`, but without the possibility to branch to a new scene. Aliased as `@|>`.
makeSceneR
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => (env -> WAG audio engine proofA res graph control)
  -> ( forall proofB
        . WAG audio engine proofB res graph control
       -> scene env audio engine proofB res
     )
  -> scene env audio engine proofA res
makeSceneR a b = makeScene (map Right a) b

infixr 6 makeSceneR as @|>

makeSceneRFlipped
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => ( forall proofB
        . WAG audio engine proofB res graph control
       -> scene env audio engine proofB res
     )
  -> (env -> WAG audio engine proofA res graph control)
  -> scene env audio engine proofA res
makeSceneRFlipped a b = makeSceneR b a

infixr 6 makeSceneRFlipped as <|@

-- | Similar to `makeSceneR'`, but without the possibility to consult an env. Aliased as `@||>`.
makeSceneR'
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => WAG audio engine proofA res graph control
  -> ( forall proofB
        . WAG audio engine proofB res graph control
       -> scene env audio engine proofB res
     )
  -> scene env audio engine proofA res
makeSceneR' a b = makeSceneR (pure a) b

infixr 6 makeSceneR' as @||>

makeSceneR'Flipped
  :: forall scene env audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IsScene scene
  => ( forall proofB
        . WAG audio engine proofB res graph control
       -> scene env audio engine proofB res
     )
  -> WAG audio engine proofA res graph control
  -> scene env audio engine proofA res
makeSceneR'Flipped a b = makeSceneR' b a

infixr 6 makeSceneR'Flipped as <||@

-- | Modifies the residual for a frame and returns the result.
-- | If a frame never modifies its residual, the value of `mempty`
-- | for `res` is returned to the scene.
modifyRes
  :: forall audio engine proof res i a
   . (res -> res)
  -> WAG audio engine proof res i a
  -> WAG audio engine proof res i res
modifyRes f w = unsafeWAG { context: (context { res = res' }), value: res' }
  where
  { context } = unsafeUnWAG w

  res' = f context.res

-- | Modifies the residual for a frame and returns the result.
-- | If a frame never modifies its residual, the value of `mempty`
-- | for `res` is returned to the scene.
imodifyRes
  :: forall audio engine proof res i
   . (res -> res)
  -> IxWAG audio engine proof res i i res
imodifyRes f = IxWAG (modifyRes f)
