module WAGS.Run where

import Prelude
import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Int (floor, toNumber)
import Data.JSDate (getTime, now)
import Data.Map as M
import Data.Set (Set)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Behavior (Behavior, sampleBy, sample_)
import FRP.Event (Event, makeEvent, subscribe)
import Record as R
import WAGS.Control.Types (Frame0, Scene, oneFrame)
import WAGS.Interpret (FFIAudio(..), FFIAudio', getAudioClockTime, renderAudio)
import WAGS.Rendered (AnAudioUnit)

type EasingAlgorithm
  = Cofree ((->) Int) Int

type EngineInfo
  = { easingAlgorithm :: EasingAlgorithm }

delay :: Int -> Event Unit
delay n =
  makeEvent \k -> do
    id <-
      setTimeout n do
        k unit
    pure (clearTimeout id)

type Run
  = { nodes :: M.Map Int AnAudioUnit
    , edges :: M.Map Int (Set Int)
    }

runInternal ::
  forall env event.
  Number ->
  { env :: { | env }
  , trigger :: event
  } ->
  Behavior { | env } ->
  Ref.Ref (Effect Unit) ->
  Ref.Ref EasingAlgorithm ->
  Ref.Ref
    ( Scene
        { time :: Number
        , env :: { | env }
        , trigger :: event
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

    fromScene = oneFrame sceneNow (R.union envAndTrigger { time })
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
    subscribe (sample_ world (delay $ max 1 remainingTimeInMs)) \env ->
      runInternal audioClockStart { env, trigger: envAndTrigger.trigger } world currentTimeoutCanceler currentEasingAlg currentScene audio' reporter
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
      eventAndEnv = sampleBy (\a b -> { trigger: b, env: a }) world trigger
    unsubscribe <-
      subscribe eventAndEnv \ee -> do
        cancelTimeout <- Ref.read currentTimeoutCanceler
        cancelTimeout
        runInternal
          audioClockStart
          ee
          world
          currentTimeoutCanceler
          currentEasingAlg
          currentScene
          audio'
          k
    pure do
      cancelTimeout <- Ref.read currentTimeoutCanceler
      cancelTimeout
      unsubscribe

{-
fiberedScene ::
  forall env audio engine proof.
  Ref Boolean ->
  env ->
  Scene env audio engine proof ->
  Event (Fiber (Scene' env audio engine proof))
fiberedScene ref env scene = affToEventizedFiber $ affifyThunkable ref next
  where
  next = oneFrameT scene env


run ::
  forall a env envWithTime.
  Cons "time" Number env envWithTime =>
  EngineInfo ->
  FFIAudio ->
  Event a ->
  Behavior { | env } ->
  Scene { | envWithTime } FFIAudio (Effect Unit) Frame0 ->
  Event (a /\ Run)
run engineInfo audio thunk world scene = ?hole
  where
  loop =
    fix \(i :: Event { jankyTime :: Instant,
    computation: Fiber (Scene' env audio engine proof) }) ->
      let
        state = sampleBy Tuple world (Tuple <$> hydrated <*> i)

        needsForce (env /\ { time, index } /\ { jankyTime })
          | time > jankyTime && index == 0 = ?hole
          | time > jankyTime = ?hole
          | otherwise = ?hole
      in
        { input: i
        , output: needsForce <$> state
        }

  hydrated =
    mapAccum
      (\x i -> Tuple (i + 1) (R.union x { index: i }))
      (withTime thunk)
      0
-}
