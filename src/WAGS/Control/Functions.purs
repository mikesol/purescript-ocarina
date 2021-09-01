module WAGS.Control.Functions
  ( start
  , istart
  , startUsing
  , startUsingWithHint
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
import Data.Tuple.Nested (type (/\))
import Control.Comonad (extract)
import Data.Either (Either(..))
import WAGS.Control.Indexed (IxWAG(..), IxFrame)
import WAGS.Control.Types (AudioState', EFrame, Frame, Frame0, InitialWAG, Scene(..), Scene', WAG, oneFrame, unsafeUnWAG, unsafeWAG)
import WAGS.CreateT (class CreateT)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Patch (class Patch, ipatch)

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
start
  :: forall assets audio engine res
   . Monoid res
  => AudioInterpret audio engine
  => InitialWAG assets audio engine res Unit
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
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => EFrame env assets audio engine proofA res { | graph } control
  -> ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> Scene env assets audio engine proofB res
     )
  -> Scene env assets audio engine proofA res
makeScene m trans = Scene go
  where
  go :: forall proofB. env -> Scene' env assets audio engine proofB res
  go env = case m env of
    Left s -> oneFrame s env
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
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> Scene env assets audio engine proofB res
     )
  -> EFrame env assets audio engine proofA res { | graph } control
  -> Scene env assets audio engine proofA res
makeSceneFlipped trans m = makeScene m trans

infixr 6 makeSceneFlipped as <@

istart
  :: forall env assets audio engine res graph control
   . Monoid res
  => AudioInterpret audio engine
  => IxFrame env assets audio engine Frame0 res {} { | graph } control
  -> ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> Scene env assets audio engine proofB res
     )
  -> Scene env assets audio engine Frame0 res
istart m = makeSceneR (\e -> let IxWAG f = m e in f start)

infixr 6 istart as @!>

startUsing
  :: forall env assets audio engine res graph control
   . Monoid res
  => AudioInterpret audio engine
  => Patch () graph
  => control
  -> ( forall proofA
        . WAG assets audio engine proofA res { | graph } control
       -> Scene env assets audio engine proofA res
     )
  -> Scene env assets audio engine Frame0 res
startUsing control next = const (ipatch $> control) @!> next

class GraphHint (i :: Type) (o :: Row Type) | i -> o

instance graphHintRec :: GraphHint { | o } o

instance graphHintTuple :: GraphHint right o => GraphHint (left /\ right) o

instance graphHintF :: GraphHint x o => GraphHint (y -> x) o

startUsingWithHint
  :: forall env assets audio engine res hintable hint graph control
   . Monoid res
  => AudioInterpret audio engine
  => GraphHint hintable hint
  => CreateT assets hint () graph
  => Patch () graph
  => hintable
  -> control
  -> ( forall proofA
        . WAG assets audio engine proofA res { | graph } control
       -> Scene env assets audio engine proofA res
     )
  -> Scene env assets audio engine Frame0 res
startUsingWithHint _ control next = const (ipatch $> control) @!> next

-- | Loops audio.
-- |
-- | The first argument is the loop and the second argument is the incoming graph that gets rendered before the loop.
-- | This means that all changes applied in the loop must be separately applied to the incoming frame if they are relevant.
-- | ```
loop
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> Frame env assets audio engine proofB res { | graph } control
     )
  -> WAG assets audio engine proofA res { | graph } control
  -> Scene env assets audio engine proofA res
loop fa ma = makeSceneR (fa ma) (loop fa)

iloop
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => ( forall proofB
        . env
       -> control
       -> IxWAG assets audio engine proofB res { | graph } { | graph } control
     )
  -> WAG assets audio engine proofA res { | graph } control
  -> Scene env assets audio engine proofA res
iloop fa = loop (\wa e -> let IxWAG f = fa e (extract wa) in f wa)

-- | Accepts a "branch" frame for making a scene, where `Left` is a new scene and `Right` is the current scene looped.
-- |
branch
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> EFrame env assets audio engine proofB res { | graph } control
     )
  -> WAG assets audio engine proofA res { | graph } control
  -> Scene env assets audio engine proofA res
branch fa w = makeScene (fa w) (branch fa)

icont
  :: forall env assets audio engine proof res graphi grapho a b
   . Monoid res
  => AudioInterpret audio engine
  => ( WAG assets audio engine proof res { | grapho } b
       -> Scene env assets audio engine proof res
     )
  -> IxWAG assets audio engine proof res { | graphi } { | grapho } b
  -> WAG assets audio engine proof res { | graphi } a
  -> Scene env assets audio engine proof res
icont c (IxWAG x) = c <<< x

ibranch
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => ( forall proofB
        . env
       -> control
       -> Either
            (WAG assets audio engine proofB res { | graph } control -> Scene env assets audio engine proofB res)
            (IxWAG assets audio engine proofB res { | graph } { | graph } control)
     )
  -> WAG assets audio engine proofA res { | graph } control
  -> Scene env assets audio engine proofA res
ibranch fa =
  branch
    ( \wa e -> case fa e (extract wa) of
        Left l -> Left $ l wa
        Right (IxWAG r) -> Right $ r wa
    )

-- | Freezes the current audio frame.
-- |
freeze
  :: forall env assets audio engine proof res graph x
   . Monoid res
  => AudioInterpret audio engine
  => WAG assets audio engine proof res { | graph } x
  -> Scene env assets audio engine proof res
freeze s = makeScene (pure $ Right s) freeze

-- | Similar to `makeScene`, but without the possibility to branch to a new scene. Aliased as `@|>`.
makeSceneR
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => Frame env assets audio engine proofA res { | graph } control
  -> ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> Scene env assets audio engine proofB res
     )
  -> Scene env assets audio engine proofA res
makeSceneR a b = makeScene (map Right a) b

infixr 6 makeSceneR as @|>

makeSceneRFlipped
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> Scene env assets audio engine proofB res
     )
  -> Frame env assets audio engine proofA res { | graph } control
  -> Scene env assets audio engine proofA res
makeSceneRFlipped a b = makeSceneR b a

infixr 6 makeSceneRFlipped as <|@

-- | Similar to `makeSceneR'`, but without the possibility to consult an env. Aliased as `@||>`.
makeSceneR'
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => WAG assets audio engine proofA res { | graph } control
  -> ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> Scene env assets audio engine proofB res
     )
  -> Scene env assets audio engine proofA res
makeSceneR' a b = makeSceneR (pure a) b

infixr 6 makeSceneR' as @||>

makeSceneR'Flipped
  :: forall env assets audio engine proofA res graph control
   . Monoid res
  => AudioInterpret audio engine
  => ( forall proofB
        . WAG assets audio engine proofB res { | graph } control
       -> Scene env assets audio engine proofB res
     )
  -> WAG assets audio engine proofA res { | graph } control
  -> Scene env assets audio engine proofA res
makeSceneR'Flipped a b = makeSceneR' b a

infixr 6 makeSceneR'Flipped as <||@

-- | Modifies the residual for a frame and returns the result.
-- | If a frame never modifies its residual, the value of `mempty`
-- | for `res` is returned to the scene.
modifyRes
  :: forall assets audio engine proof res i a
   . AudioInterpret audio engine
  => (res -> res)
  -> WAG assets audio engine proof res i a
  -> WAG assets audio engine proof res i res
modifyRes f w = unsafeWAG { context: (context { res = res' }), value: res' }
  where
  { context } = unsafeUnWAG w

  res' = f context.res

-- | Modifies the residual for a frame and returns the result.
-- | If a frame never modifies its residual, the value of `mempty`
-- | for `res` is returned to the scene.
imodifyRes
  :: forall assets audio engine proof res i
   . AudioInterpret audio engine
  => (res -> res)
  -> IxWAG assets audio engine proof res i i res
imodifyRes f = IxWAG (modifyRes f)
