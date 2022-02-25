-- | Run a `Scene` to produce sound using the Web Audio API.
module WAGS.Run
  ( EasingAlgorithm
  , EngineInfo
  , Run
  , TriggeredScene(..)
  , BehavingScene(..)
  , SharedScene
  , RunAudio
  , RunEngine
  , class MakeAnalyserCallbacks
  , makeAnalyserCallbacks
  , class AnalyserRefs
  , makeAnalyserRefs
  , class Analysers
  , getAnalysers
  , bufferToList
  , run
  , runNoLoop
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import FRP.Behavior (Behavior, sampleBy, sample_)
import FRP.Behavior.Time (instant)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Time (withTime, delay)
import Prim.Row (class Lacks)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Frame0, Scene, oneFrame)
import WAGS.Interpret (FFIAudioSnapshot, advanceWriteHead, contextFromSnapshot, getAudioClockTime, renderAudio)
import WAGS.Rendered (Instruction)
import WAGS.WebAPI as WebAPI

type RunAudio
  = Unit /\ FFIAudioSnapshot

type RunEngine
  = Instruction /\ Effect Unit

class Analysers (analysersRL :: RL.RowList Type) analyserRefs analysers | analysersRL analyserRefs -> analysers where
  getAnalysers :: forall proxy. proxy analysersRL -> { | analyserRefs } -> Effect { | analysers }

instance getAnalysersNil :: Analysers RL.Nil analyserRefs () where
  getAnalysers _ _ = pure {}

instance getAnalysersCons ::
  ( IsSymbol key
  , Row.Cons key (Ref.Ref (Maybe WebAPI.AnalyserNode)) analyserRefs' analyserRefs
  , Row.Cons key (Maybe WebAPI.AnalyserNode) analysers' analysers
  , Lacks key analyserRefs'
  , Lacks key analysers'
  , Analysers rest analyserRefs analysers'
  ) =>
  Analysers (RL.Cons key (Maybe WebAPI.AnalyserNode) rest) analyserRefs analysers where
  getAnalysers _ refs = do
    mbe <- Ref.read (Record.get (Proxy :: _ key) refs)
    Record.insert (Proxy :: _ key) mbe <$> (getAnalysers (Proxy :: _ rest) refs)

class AnalyserRefs (analysersRL :: RL.RowList Type) analyserRefs | analysersRL -> analyserRefs where
  makeAnalyserRefs :: forall proxy. proxy analysersRL -> Effect { | analyserRefs }

instance analyserRefsNil :: AnalyserRefs RL.Nil () where
  makeAnalyserRefs _ = pure {}

instance analyserRefsCons ::
  ( IsSymbol key
  , Row.Cons key (Ref.Ref (Maybe WebAPI.AnalyserNode)) analyserRefs' analyserRefs
  , Lacks key analyserRefs'
  , AnalyserRefs rest analyserRefs'
  ) =>
  AnalyserRefs (RL.Cons key (Maybe WebAPI.AnalyserNode) rest) analyserRefs where
  makeAnalyserRefs _ = do
    ref <- Ref.new Nothing
    Record.insert (Proxy :: _ key) ref <$> (makeAnalyserRefs (Proxy :: _ rest))

class MakeAnalyserCallbacks (analysersRL :: RL.RowList Type) analyserRefs analyserCallbacks | analysersRL analyserRefs -> analyserCallbacks where
  makeAnalyserCallbacks :: forall proxy. proxy analysersRL -> { | analyserRefs } -> { | analyserCallbacks }

instance workWithAnalysersNil :: MakeAnalyserCallbacks RL.Nil analyserRefs () where
  makeAnalyserCallbacks _ _ = {}

instance workWithAnalysersCons ::
  ( IsSymbol key
  , Row.Cons key (Ref.Ref (Maybe WebAPI.AnalyserNode)) analyserRefs' analyserRefs
  , Row.Cons key WebAPI.AnalyserNodeCb analyserCallbacks' analyserCallbacks
  , Lacks key analyserRefs'
  , Lacks key analyserCallbacks'
  , MakeAnalyserCallbacks rest analyserRefs analyserCallbacks'
  ) =>
  MakeAnalyserCallbacks (RL.Cons key (Maybe WebAPI.AnalyserNode) rest) analyserRefs analyserCallbacks where
  makeAnalyserCallbacks _ refs = Record.insert
    (Proxy :: _ key)
    (WebAPI.AnalyserNodeCb f)
    (makeAnalyserCallbacks (Proxy :: _ rest) refs)
    where
    ref = Record.get (Proxy :: _ key) refs
    f :: WebAPI.AnalyserNode -> Effect (Effect Unit)
    f node = do
      Ref.write (Just node) ref
      pure (Ref.write Nothing ref)

-- | Run a scene.
-- |
-- | - `Event trigger` is the event to which the scene reacts. `trigger` will contain things like an initial event, mouse clicks, MIDI onsets, OSC commands and any other event to which the scene should respond.  Because of this, the polymorphic type `trigger` is often defined as an ADT with different potential incoming actions, similar to how [actions are defined in Halogen](https://github.com/purescript-halogen/purescript-halogen/blob/master/docs/guide/02-Introducing-Components.md#actions). Note that no sound will be produced unless there is _at least_ one event. For this reason, there is usually some form of initial event, ie `data Trigger = InitialEvent | MouseClick | etc..`, that is sent to start audio rendering. All of the examples in this repo contain an initial event, which is often `pure unit` in the case where there in _only_ the initial event.
-- | - `Behavior world` is the outside environment. `world` will usually contain things like the current mouse position, the ambient temperature, the axial tilt of the Earth, or other things that can be modeled as a continuous function of time. One important thing to note is that `world` _lags_ `trigger` by 0 or 1 events in the [browser event queue](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop). For most real-world applications, this does not matter, but it does lead to subtle logic bugs if `trigger` and `world` are corrolated. For this reason, it is good to decouple `trigger` and `world`.
-- | - `EngineInfo` is the engine information needed for rendering.
-- | - `FFIAudio` is the audio state needed for rendering
-- | - `Scene` is the scene to render. See `BehavingScene` to understand how `trigger` and `world` are blended into the inptu environment going to `Scene`.
type RunSig sceneType analysersRL analysers analyserCallbacks analyserRefs trigger world res = RL.RowToList analysers analysersRL
  => AnalyserRefs analysersRL analyserRefs
  => MakeAnalyserCallbacks analysersRL analyserRefs analyserCallbacks
  => Analysers analysersRL analyserRefs analysers
  => Monoid res
  => Event trigger
  -> Behavior world
  -> EngineInfo
  -> FFIAudioSnapshot
  -> Scene (sceneType trigger world analyserCallbacks)
       RunAudio
       RunEngine
       Frame0
       res
  -> Event (Run res analysers)


-- TODO: so much code duplication here. fix...
runNoLoop :: forall analysersRL analysers analyserCallbacks analyserRefs trigger world res. RunSig TriggeredScene analysersRL analysers analyserCallbacks analyserRefs trigger world res
runNoLoop trigger inWorld engineInfo audioInfo scene =
  makeEvent \k -> do
    refsForAnalysers <- makeAnalyserRefs (Proxy :: _ analysersRL)
    currentTimeoutCanceler <- Ref.new (pure unit :: Effect Unit)
    currentScene <- Ref.new scene
    currentEasingAlg <- Ref.new engineInfo.easingAlgorithm
    let
      newWorld =
        { world: _
        , sysTime: _
        }
          <$> inWorld
          <*> map unInstant instant
      eventAndEnv =
        sampleBy
          ( \{ world
             , sysTime
             }
             b ->
              { trigger: b
              , world
              , sysTime
              }
          )
          newWorld
          trigger
    unsubscribe <-
      subscribe eventAndEnv \ee -> do
        cancelTimeout <- Ref.read currentTimeoutCanceler
        cancelTimeout
        runInternal
          { audioClockStart: Nothing, writeHeadStart: Nothing }
          ee
          newWorld
          currentTimeoutCanceler
          currentEasingAlg
          currentScene
          audioInfo
          (makeAnalyserCallbacks (Proxy :: _ analysersRL) refsForAnalysers)
          refsForAnalysers
          k
    pure do
      cancelTimeout <- Ref.read currentTimeoutCanceler
      cancelTimeout
      unsubscribe
  where
  runInternal clockInfo fromEvents world' currentTimeoutCanceler currentEasingAlg currentScene ffiSnapshot analyserCallbacks analyserRefs reporter = do
    easingAlgNow <- Ref.read currentEasingAlg
    sceneNow <- Ref.read currentScene
    let
      ctx = contextFromSnapshot ffiSnapshot
      -- this is how far in the future we are telling the
      -- algorithm to calculate with respect to the audio clock
      -- it is a bet:  we bet that the algorithm will finish
      -- in this amount of time
      headroom = head easingAlgNow

      headroomInSeconds = (toNumber headroom) / 1000.0

    audioClockPriorToComputation <- getAudioClockTime ctx
    let
      audioClockStart = fromMaybe audioClockPriorToComputation clockInfo.audioClockStart
      time = audioClockPriorToComputation - audioClockStart

      fromScene =
        oneFrame sceneNow
          ( TriggeredScene
              { world: fromEvents.world
              , trigger: fromEvents.trigger
              , sysTime: fromEvents.sysTime
              , time
              , headroom
              , headroomInSeconds
              , analyserCallbacks
              }
          )
    audioClockAfterComputation <- getAudioClockTime ctx
    let
      writeHeadStart = fromMaybe (audioClockAfterComputation + headroomInSeconds) clockInfo.writeHeadStart
      ffi = advanceWriteHead ffiSnapshot $ (writeHeadStart + time)
      applied = map ((#) (unit /\ ffi)) fromScene.instructions
    renderAudio (map snd applied)
    let
      remainingTimeInSeconds = audioClockPriorToComputation + headroomInSeconds - audioClockAfterComputation
      remainingTime = floor $ 1000.0 * remainingTimeInSeconds
    Ref.write (tail easingAlgNow remainingTime) currentEasingAlg
    Ref.write
      fromScene.next
      currentScene
    analysers <- getAnalysers (Proxy :: _ analysersRL) analyserRefs
    reporter
      { instructions: map fst applied
      , res: fromScene.res
      , remainingTimeInSeconds
      , remainingTime
      , headroom
      , headroomInSeconds
      , analysers
      }

run :: forall analysersRL analysers analyserCallbacks analyserRefs trigger world res. RunSig BehavingScene analysersRL analysers analyserCallbacks analyserRefs trigger world res
run trigger inWorld engineInfo audioInfo scene =
  makeEvent \k -> do
    refsForAnalysers <- makeAnalyserRefs (Proxy :: _ analysersRL)
    currentTimeoutCanceler <- Ref.new (pure unit :: Effect Unit)
    currentScene <- Ref.new scene
    currentEasingAlg <- Ref.new engineInfo.easingAlgorithm
    let
      newWorld =
        { world: _
        , sysTime: _
        }
          <$> inWorld
          <*> map unInstant instant
      eventAndEnv =
        sampleBy
          ( \{ world
             , sysTime
             }
             b ->
              { trigger: Just b
              , world
              , sysTime
              }
          )
          newWorld
          trigger
    unsubscribe <-
      subscribe eventAndEnv \ee -> do
        cancelTimeout <- Ref.read currentTimeoutCanceler
        cancelTimeout
        runInternal
          { audioClockStart: Nothing, writeHeadStart: Nothing }
          ee
          newWorld
          currentTimeoutCanceler
          currentEasingAlg
          currentScene
          audioInfo
          (makeAnalyserCallbacks (Proxy :: _ analysersRL) refsForAnalysers)
          refsForAnalysers
          k
    pure do
      cancelTimeout <- Ref.read currentTimeoutCanceler
      cancelTimeout
      unsubscribe
  where
  runInternal clockInfo fromEvents world' currentTimeoutCanceler currentEasingAlg currentScene ffiSnapshot analyserCallbacks analyserRefs reporter = do
    easingAlgNow <- Ref.read currentEasingAlg
    sceneNow <- Ref.read currentScene
    let
      ctx = contextFromSnapshot ffiSnapshot
      -- this is how far in the future we are telling the
      -- algorithm to calculate with respect to the audio clock
      -- it is a bet:  we bet that the algorithm will finish
      -- in this amount of time
      headroom = head easingAlgNow

      headroomInSeconds = (toNumber headroom) / 1000.0

    audioClockPriorToComputation <- getAudioClockTime ctx
    let
      audioClockStart = fromMaybe audioClockPriorToComputation clockInfo.audioClockStart
      time = audioClockPriorToComputation - audioClockStart

      fromScene =
        oneFrame sceneNow
          ( BehavingScene
              { world: fromEvents.world
              , trigger: fromEvents.trigger
              , sysTime: fromEvents.sysTime
              , time
              , headroom
              , headroomInSeconds
              , analyserCallbacks
              }
          )
    audioClockAfterComputation <- getAudioClockTime ctx
    let
      writeHeadStart = fromMaybe (audioClockAfterComputation + headroomInSeconds) clockInfo.writeHeadStart
      ffi = advanceWriteHead ffiSnapshot $ (writeHeadStart + time)
      applied = map ((#) (unit /\ ffi)) fromScene.instructions
    renderAudio (map snd applied)
    let
      remainingTimeInSeconds = audioClockPriorToComputation + headroomInSeconds - audioClockAfterComputation
      remainingTime = floor $ 1000.0 * remainingTimeInSeconds
    Ref.write (tail easingAlgNow remainingTime) currentEasingAlg
    Ref.write
      fromScene.next
      currentScene
    analysers <- getAnalysers (Proxy :: _ analysersRL) analyserRefs
    reporter
      { instructions: map fst applied
      , res: fromScene.res
      , remainingTimeInSeconds
      , remainingTime
      , headroom
      , headroomInSeconds
      , analysers
      }
    -- we thunk the world and move on to the next event
    -- note that if we did not allocate enough time, we still
    -- set a timeout of 1 so that the canceler can run in case it needs to
    canceler <-
      subscribe (sample_ world' (delay (max 1 remainingTime) (pure unit)))
        \{ world
          , sysTime
          } ->
          runInternal { audioClockStart: Just audioClockStart, writeHeadStart: Just writeHeadStart }
            { world
            , sysTime
            , trigger: Nothing
            }
            world'
            currentTimeoutCanceler
            currentEasingAlg
            currentScene
            ffiSnapshot
            analyserCallbacks
            analyserRefs
            reporter
    Ref.write canceler currentTimeoutCanceler

-- | The information provided to `run` that tells the engine how to make certain rendering tradeoffs.
type EngineInfo
  = { easingAlgorithm :: EasingAlgorithm }

-- | An algorithm that tells the engine how much lookahead the audio should have in milliseconds. The `(->) Int` is a penalty function, where a positive input is the number of milliseconds left over after rendering (meaning we gave too much headroom) and a negative input is the number of milliseconds by which we missed the deadline (meaning there was not enough headroom). This allows the algorithm to make adjustments if necessary.
-- |
-- | As an example:
-- |
-- | ```purescript
-- | easingAlgorithm :: EasingAlgorithm
-- | easingAlgorithm =
-- |   let
-- |     fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
-- |   in
-- |     fOf 20
-- | ```
-- |
-- | This easing algorithm always provides at least 20ms of headroom to the algorithm, but adjusts upwards in case deadlines are being missed.
type EasingAlgorithm
  = Cofree ((->) Int) Int

type Run res analysers
  =
  { instructions :: Array Instruction
  , res :: res
  , remainingTimeInSeconds :: Number
  , remainingTime :: Int
  , headroomInSeconds :: Number
  , headroom :: Int
  , analysers :: { | analysers }
  }

-- | The input type to a scene that is handled by `run`. Given `Event trigger` and `Behavior world`, the scene will receive:
-- |
-- | `trigger` - the trigger. If none exists (meaning we are polling) it will be Nothing.
-- | `world` - the world.
-- | `time` - the time of the audio context.
-- | `sysTime` - the time provided by `new Date().getTime()`
-- | `headroom` - the amount of lookahead time. If you are programming a precise rhythmic event and need the onset to occur at a specific moment, you can use `headroom` to determine if the apex should happen now or later.

type SharedScene world analyserCallbacks =
  ( world :: world
  , time :: Number
  , sysTime :: Milliseconds
  , headroom :: Int
  , headroomInSeconds :: Number
  , analyserCallbacks :: { | analyserCallbacks }
  )

newtype BehavingScene trigger world analyserCallbacks
  = BehavingScene
  { trigger :: Maybe trigger
  | SharedScene world analyserCallbacks
  }

newtype TriggeredScene trigger world analyserCallbacks
  = TriggeredScene
  { trigger :: trigger
  | SharedScene world analyserCallbacks
  }

derive instance newtypeBehavingScene :: Newtype (BehavingScene trigger world analyserCallbacks) _

-- | Given a buffering window and an event, return a list of events that occur within that window.
-- | - `timeToCollect` - the buffering window
-- | - `incomingEvent` - the event to buffer
-- |
-- | For example, if `event` outputs the following sequence:
-- |
-- | - `unit` @ 0 ms
-- | - `unit` @ 1 ms
-- | - `unit` @ 7 ms
-- | - `unit` @ 9 ms
-- | - `unit` @ 15 ms
-- |
-- | Then:
-- |
-- | ```purescript
-- | bufferToList 4 event
-- | ```
-- |
-- | would group together events within the same 4ms window before emitting them, resulting in
-- |
-- | ```purescript
-- | { time :: 0ms, value :: unit } : { time :: 1ms, value :: unit } : Nil -- emitted at 4ms
-- | { time :: 7ms, value :: unit } : { time :: 9ms, value :: unit } : Nil -- emitted at 11ms
-- | { time :: 15ms, value :: unit } : Nil -- emitted at 19ms
-- | ```
bufferToList
  :: forall a
   . Int
  -> Event a
  -> Event (List { time :: Instant, value :: a })
bufferToList timeToCollect incomingEvent =
  makeEvent \k -> do
    currentTimeoutId <- Ref.new (Nothing :: Maybe TimeoutId)
    currentEventList <- Ref.new (Nil :: List { time :: Instant, value :: a })
    unsub <- subscribe timed \a -> do
      Ref.modify_ (Cons a) currentEventList
      inTimeout <- Ref.read currentTimeoutId
      when (isNothing inTimeout) $ (flip Ref.write currentTimeoutId <<< Just)
        =<< setTimeout timeToCollect do
          cil <- Ref.read currentEventList
          Ref.write Nil currentEventList
          Ref.write Nothing currentTimeoutId
          k cil
    pure do
      Ref.read currentTimeoutId >>= flip for_ clearTimeout
      unsub
  where
  timed = withTime incomingEvent
