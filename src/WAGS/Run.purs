module WAGS.Run where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.DateTime.Instant (Instant)
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.JSDate (getTime, now)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import FRP.Behavior (Behavior, sampleBy, sample_)
import FRP.Behavior.Time (instant)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Time (withTime, delay)
import Record as R
import WAGS.Control.Types (Frame0, Scene, oneFrame)
import WAGS.Interpret (FFIAudio(..), FFIAudio', getAudioClockTime, renderAudio)
import WAGS.Rendered (AnAudioUnit)

type EasingAlgorithm
  = Cofree ((->) Int) Int

type EngineInfo
  = { easingAlgorithm :: EasingAlgorithm }

type Run
  = { nodes :: M.Map Int AnAudioUnit
    , edges :: M.Map Int (Set Int)
    }

bufferToList ::
  forall a.
  Int ->
  Event a ->
  Event (List { time :: Instant, value :: a })
bufferToList timeToCollect incomingEvent =
  makeEvent \k -> do
    currentTimeoutId <- Ref.new (Nothing :: Maybe TimeoutId)
    currentEventList <- Ref.new (Nil :: List { time :: Instant, value :: a })
    subscribe timed \a -> do
      Ref.modify_ (Cons a) currentEventList
      inTimeout <- Ref.read currentTimeoutId
      case inTimeout of
        Just x -> pure unit
        Nothing -> do
          id <-
            setTimeout timeToCollect do
              cil <- Ref.read currentEventList
              Ref.write Nil currentEventList
              Ref.write Nothing currentTimeoutId
              k cil
          Ref.write (Just id) currentTimeoutId
      pure $ Ref.read currentTimeoutId >>= flip for_ clearTimeout 
  where
  timed = withTime incomingEvent

type SceneI trigger world
  = { time :: Number
    , world :: world
    , trigger :: trigger
    , sysTime :: Instant
    , active :: Boolean
    , headroom :: Int
    }

runInternal ::
  forall world trigger.
  Number ->
  { world :: world
  , trigger :: trigger
  , sysTime :: Instant
  , active :: Boolean
  } ->
  Behavior { world :: world, sysTime :: Instant } ->
  Ref.Ref (Effect Unit) ->
  Ref.Ref EasingAlgorithm ->
  Ref.Ref
    ( Scene
        (SceneI trigger world)
        FFIAudio
        (Effect Unit)
        Frame0
    ) ->
  FFIAudio' ->
  (Run -> Effect Unit) ->
  Effect Unit
runInternal audioClockStart worldAndTrigger world' currentTimeoutCanceler currentEasingAlg currentScene audio' reporter = do
  easingAlgNow <- Ref.read currentEasingAlg
  sceneNow <- Ref.read currentScene
  audioClockPriorToComputation <- getAudioClockTime audio'.context
  let
    -- this is how far in the future we are telling the
    -- algorithm to calculate with respect to the audio clock
    -- it is a bet: we bet that the algorithm will finish in this amount
    -- of time
    headroom = head easingAlgNow

    headroomInSeconds = (toNumber headroom) / 1000.0

    time = (audioClockPriorToComputation - audioClockStart) + headroomInSeconds

    fromScene = oneFrame sceneNow (R.union worldAndTrigger { time, headroom })
  audioClockAfterComputation <- getAudioClockTime audio'.context
  renderAudio
    ( FFIAudio
        $ audio'
            { writeHead = max audioClockAfterComputation (audioClockPriorToComputation + headroomInSeconds)
            }
    )
    fromScene.instructions
  let
    remainingTimeInMs = floor $ 1000.0 * (audioClockPriorToComputation + headroomInSeconds - audioClockAfterComputation)
  Ref.write (tail easingAlgNow remainingTimeInMs) currentEasingAlg
  Ref.write
    fromScene.next
    currentScene
  reporter { nodes: fromScene.nodes, edges: fromScene.edges }
  -- we thunk the world and move on to the next event
  -- note that if we did not allocate enough time, we still
  -- set a timeout of 1 so that th canceler can run in case it needs to
  canceler <-
    subscribe (sample_ world' (delay (max 1 remainingTimeInMs) (pure unit))) \{ world, sysTime } ->
      runInternal audioClockStart { world, sysTime, trigger: worldAndTrigger.trigger, active: false } world' currentTimeoutCanceler currentEasingAlg currentScene audio' reporter
  Ref.write canceler currentTimeoutCanceler

type RunSig world trigger
  = Event trigger ->
    Behavior world ->
    EngineInfo ->
    FFIAudio ->
    Scene
      (SceneI trigger world)
      FFIAudio
      (Effect Unit)
      Frame0 ->
    Event Run

run :: forall world trigger. RunSig world trigger
run trigger world' engineInfo audio@(FFIAudio audio') scene =
  makeEvent \k -> do
    audioClockStart <- getAudioClockTime audio'.context
    clockClockStart <- map ((_ / 1000.0) <<< getTime) now
    currentTimeoutCanceler <- Ref.new (pure unit :: Effect Unit)
    currentScene <- Ref.new scene
    currentEasingAlg <- Ref.new engineInfo.easingAlgorithm
    let
      eventAndEnv = sampleBy (\{ world, sysTime } b -> { trigger: b, world, sysTime, active: true }) newWorld trigger
    unsubscribe <-
      subscribe eventAndEnv \ee -> do
        cancelTimeout <- Ref.read currentTimeoutCanceler
        cancelTimeout
        runInternal
          audioClockStart
          ee
          newWorld
          currentTimeoutCanceler
          currentEasingAlg
          currentScene
          audio'
          k
    pure do
      cancelTimeout <- Ref.read currentTimeoutCanceler
      cancelTimeout
      unsubscribe
  where
  newWorld = (\world sysTime -> { world, sysTime }) <$> world' <*> instant
