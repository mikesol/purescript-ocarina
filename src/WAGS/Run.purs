module WAGS.Run where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.DateTime.Instant (Instant)
import Data.Int (floor, toNumber)
import Data.JSDate (getTime, now)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import FRP.Behavior (Behavior, sampleBy, sample_)
import FRP.Behavior.Time (instant)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Time (withTime)
import Record as R
import WAGS.Control.Types (Frame0, Scene, oneFrame)
import WAGS.Interpret (FFIAudio(..), FFIAudio', getAudioClockTime, renderAudio)
import WAGS.Rendered (AnAudioUnit)

type EasingAlgorithm
  = Cofree ((->) Int) Int

type EngineInfo
  = { easingAlgorithm :: EasingAlgorithm }

delayEvent :: Int -> Event Unit
delayEvent n =
  makeEvent \k -> do
    id <-
      setTimeout n do
        k unit
    pure (clearTimeout id)

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
      pure do
        maybeId <- Ref.read currentTimeoutId
        maybe (pure unit) clearTimeout maybeId
  where
  timed = withTime incomingEvent

runInternal ::
  forall env event.
  Number ->
  { env :: env
  , trigger :: event
  , sysTime :: Instant
  , active :: Boolean
  } ->
  Behavior { env :: env, sysTime :: Instant } ->
  Ref.Ref (Effect Unit) ->
  Ref.Ref EasingAlgorithm ->
  Ref.Ref
    ( Scene
        { time :: Number
        , env :: env
        , trigger :: event
        , sysTime :: Instant
        , headroom :: Int
        , active :: Boolean
        }
        FFIAudio
        (Effect Unit)
        Frame0
    ) ->
  FFIAudio' ->
  (Run -> Effect Unit) ->
  Effect Unit
runInternal audioClockStart envAndTrigger world currentTimeoutCanceler currentEasingAlg currentScene audio' reporter = do
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

    fromScene = oneFrame sceneNow (R.union envAndTrigger { time, headroom })
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
    subscribe (sample_ world (delayEvent $ max 1 remainingTimeInMs)) \{ env, sysTime } ->
      runInternal audioClockStart { env, sysTime, trigger: envAndTrigger.trigger, active: false } world currentTimeoutCanceler currentEasingAlg currentScene audio' reporter
  Ref.write canceler currentTimeoutCanceler

run ::
  forall env event.
  EngineInfo ->
  FFIAudio ->
  Event event ->
  Behavior { | env } ->
  Scene
    { time :: Number
    , env :: { | env }
    , trigger :: event
    , sysTime :: Instant
    , active :: Boolean
    , headroom :: Int
    }
    FFIAudio
    (Effect Unit)
    Frame0 ->
  Event Run
run engineInfo audio@(FFIAudio audio') trigger world scene =
  makeEvent \k -> do
    audioClockStart <- getAudioClockTime audio'.context
    clockClockStart <- map ((_ / 1000.0) <<< getTime) now
    currentTimeoutCanceler <- Ref.new (pure unit :: Effect Unit)
    currentScene <- Ref.new scene
    currentEasingAlg <- Ref.new engineInfo.easingAlgorithm
    let
      eventAndEnv = sampleBy (\{ env, sysTime } b -> { trigger: b, env, sysTime, active: true }) newWorld trigger
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
  newWorld = (\env sysTime -> { env, sysTime }) <$> world <*> instant
