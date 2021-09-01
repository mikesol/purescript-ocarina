-- | Run a `Scene` to produce sound using the Web Audio API.
module WAGS.Run
  ( EasingAlgorithm
  , EngineInfo
  , Run
  , SceneI(..)
  , RunAudio
  , RunEngine
  , class WorkWithAnalysers
  , class AreAudioWorklets
  , initializeAnalysers
  , makeAnalyserUpdaters
  , bufferToList
  , run
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Lens as Lens
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Nat, class Pos)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import FRP.Behavior (Behavior, behavior, sampleBy, sample_)
import FRP.Behavior.Time (instant)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Time (withTime, delay)
import Foreign (Foreign)
import Foreign.Object as O
import Prim.Row (class Lacks)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Safe.Coerce (coerce)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Control.Types (Frame0, Scene, oneFrame)
import WAGS.Graph.AudioUnit (AudioWorkletNode)
import WAGS.Graph.Parameter (AudioParameter)
import WAGS.Interpret (AnalyserNode, AudioContext, AudioWorkletNodeProxy, BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, FFIAudioSnapshot(..), MediaRecorder, getAudioClockTime, renderAudio)
import WAGS.Rendered (Instruction)
import WAGS.Util (class ValidateOutputChannelCount)

type RunAudio
  = Unit /\ FFIAudioSnapshot

type RunEngine
  = Instruction /\ Effect Unit

class WorkWithAnalysers (analysersRL :: RL.RowList Type) initializedAnalysers | analysersRL -> initializedAnalysers where
  initializeAnalysers :: forall proxy. proxy analysersRL -> { | initializedAnalysers }
  makeAnalyserUpdaters :: forall proxy. proxy analysersRL -> Ref.Ref { | initializedAnalysers } -> O.Object (AnalyserNode -> Effect (Effect Unit))

instance workWithAnalysersNil :: WorkWithAnalysers RL.Nil () where
  initializeAnalysers _ = {}
  makeAnalyserUpdaters _ _ = O.empty

instance workWithAnalysersCons ::
  ( IsSymbol key
  , Row.Cons key (Maybe AnalyserNode) initializedAnalysers' initializedAnalysers
  , Lacks key initializedAnalysers'
  , WorkWithAnalysers rest initializedAnalysers'
  ) =>
  WorkWithAnalysers (RL.Cons key (Maybe AnalyserNode) rest) initializedAnalysers where
  initializeAnalysers _ = Record.insert (Proxy :: _ key) Nothing (initializeAnalysers (Proxy :: _ rest))
  makeAnalyserUpdaters _ initializedAnalysers =
    O.insert
      (reflectSymbol (Proxy :: _ key))
      f
      -- unsafeCoerce safe here because use of unsafeSet in prop, which iterates over whole record
      -- TODO: find better way to handle this?
      (makeAnalyserUpdaters (Proxy :: _ rest) (unsafeCoerce initializedAnalysers))
    where
    f :: AnalyserNode -> Effect (Effect Unit)
    f node = do
      void $ Ref.modify (Lens.set (prop (Proxy :: _ key)) (Just node)) initializedAnalysers
      pure (void $ Ref.modify (Lens.set (prop (Proxy :: _ key)) Nothing) initializedAnalysers)

refToBehavior :: Ref.Ref ~> Behavior
refToBehavior r = behavior \e -> makeEvent \k -> subscribe e \f -> Ref.read r >>= (k <<< f)

class AreAudioWorklets (workletsRL :: RL.RowList Type)

instance areAudioWorkletsNil :: AreAudioWorklets RL.Nil

instance areAudioWorkletsCons ::
  ( Nat numberOfInputs
  , Pos numberOfOutputs
  , ValidateOutputChannelCount numberOfOutputs outputChannelCount
  , Homogeneous parameterData AudioParameter
  , JSON.WriteForeign { | processorOptions }
  , AreAudioWorklets rest
  ) =>
  AreAudioWorklets (RL.Cons key (AudioWorkletNodeProxy (AudioWorkletNode name numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions)) rest)

-- | Run a scene.
-- |
-- | - `Event trigger` is the event to which the scene reacts. `trigger` will contain things like an initial event, mouse clicks, MIDI onsets, OSC commands and any other event to which the scene should respond.  Because of this, the polymorphic type `trigger` is often defined as an ADT with different potential incoming actions, similar to how [actions are defined in Halogen](https://github.com/purescript-halogen/purescript-halogen/blob/master/docs/guide/02-Introducing-Components.md#actions). Note that no sound will be produced unless there is _at least_ one event. For this reason, there is usually some form of initial event, ie `data Trigger = InitialEvent | MouseClick | etc..`, that is sent to start audio rendering. All of the examples in this repo contain an initial event, which is often `pure unit` in the case where there in _only_ the initial event.
-- | - `Behavior world` is the outside environment. `world` will usually contain things like the current mouse position, the ambient temperature, the axial tilt of the Earth, or other things that can be modeled as a continuous function of time. One important thing to note is that `world` _lags_ `trigger` by 0 or 1 events in the [browser event queue](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop). For most real-world applications, this does not matter, but it does lead to subtle logic bugs if `trigger` and `world` are corrolated. For this reason, it is good to decouple `trigger` and `world`.
-- | - `EngineInfo` is the engine information needed for rendering.
-- | - `FFIAudio` is the audio state needed for rendering
-- | - `Scene` is the scene to render. See `SceneI` to understand how `trigger` and `world` are blended into the inptu environment going to `Scene`.
run
  :: forall analysersRL analysers recorders buffers floatArrays periodicWaves worklets workletsRL trigger world res
   . Homogeneous recorders (MediaRecorder -> Effect Unit)
  => Homogeneous buffers BrowserAudioBuffer
  => Homogeneous floatArrays BrowserFloatArray
  => Homogeneous periodicWaves BrowserPeriodicWave
  => RL.RowToList worklets workletsRL
  => AreAudioWorklets workletsRL
  => RL.RowToList analysers analysersRL
  => WorkWithAnalysers analysersRL analysers
  => Monoid res
  => Event trigger
  -> Behavior world
  -> EngineInfo
  -> { context :: AudioContext
     , writeHead :: Number
     , units :: Foreign
     , microphone :: Behavior (Nullable BrowserMicrophone)
     , worklets :: { | worklets }
     , recorders :: Behavior { | recorders }
     , buffers :: Behavior { | buffers }
     , floatArrays :: Behavior { | floatArrays }
     , periodicWaves :: Behavior { | periodicWaves }
     }
  -> Scene (SceneI trigger world)
       ( buffers :: { | buffers }
       , recorders :: { | recorders }
       , floatArrays :: { | floatArrays }
       , periodicWaves :: { | periodicWaves }
       , analysers :: { | analysers }
       , worklets :: { | worklets }
       )
       RunAudio
       RunEngine
       Frame0
       res
  -> Event (Run res analysers)
run trigger world' engineInfo audioWithBehaviors scene =
  makeEvent \k -> do
    refForAnalysers <- Ref.new (initializeAnalysers (Proxy :: _ analysersRL))
    audioClockStart <- getAudioClockTime audioWithBehaviors.context
    currentTimeoutCanceler <- Ref.new (pure unit :: Effect Unit)
    currentScene <- Ref.new scene
    currentEasingAlg <- Ref.new engineInfo.easingAlgorithm
    let
      newWorld =
        { world: _
        , sysTime: _
        , microphone: _
        , analysers: _
        , recorders: _
        , buffers: _
        , floatArrays: _
        , periodicWaves: _
        }
          <$> world'
          <*> map unInstant instant
          <*> audioWithBehaviors.microphone
          <*> refToBehavior refForAnalysers
          <*> map O.fromHomogeneous audioWithBehaviors.recorders
          <*> map O.fromHomogeneous audioWithBehaviors.buffers
          <*> map O.fromHomogeneous audioWithBehaviors.floatArrays
          <*> map O.fromHomogeneous audioWithBehaviors.periodicWaves

      eventAndEnv =
        sampleBy
          ( \{ world
             , sysTime
             , microphone
             , analysers
             , recorders
             , buffers
             , floatArrays
             , periodicWaves
             }
             b ->
              { trigger: Just b
              , world
              , sysTime
              , microphone
              , analysers
              , recorders
              , buffers
              , floatArrays
              , periodicWaves
              }
          )
          newWorld
          trigger
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
          (makeAnalyserUpdaters (Proxy :: _ analysersRL) refForAnalysers)
          { context: audioWithBehaviors.context
          , units: audioWithBehaviors.units
          , writeHead: audioWithBehaviors.writeHead
          }
          k
    pure do
      cancelTimeout <- Ref.read currentTimeoutCanceler
      cancelTimeout
      unsubscribe

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

type NonBehavioralFFIInfo
  =
  { context :: AudioContext
  , writeHead :: Number
  , units :: Foreign
  }

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
newtype SceneI trigger world
  = SceneI
  { trigger :: Maybe trigger
  , world :: world
  , time :: Number
  , sysTime :: Milliseconds
  , headroom :: Int
  , headroomInSeconds :: Number
  }

derive instance newtypeSceneI :: Newtype (SceneI trigger world) _

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
    subscribe timed \a -> do
      Ref.modify_ (Cons a) currentEventList
      inTimeout <- Ref.read currentTimeoutId
      when (isNothing inTimeout) $ (flip Ref.write currentTimeoutId <<< Just)
        =<< setTimeout timeToCollect do
          cil <- Ref.read currentEventList
          Ref.write Nil currentEventList
          Ref.write Nothing currentTimeoutId
          k cil
      pure $ Ref.read currentTimeoutId >>= flip for_ clearTimeout
  where
  timed = withTime incomingEvent

runInternal
  :: forall analysers assets trigger world res
   . Monoid res
  => Number
  -> { world :: world
     , trigger :: Maybe trigger
     , sysTime :: Milliseconds
     , microphone :: Nullable BrowserMicrophone
     , analysers :: { | analysers }
     , recorders :: O.Object (MediaRecorder -> Effect Unit)
     , buffers :: O.Object BrowserAudioBuffer
     , floatArrays :: O.Object BrowserFloatArray
     , periodicWaves :: O.Object BrowserPeriodicWave
     }
  -> Behavior
       { world :: world
       , sysTime :: Milliseconds
       , microphone :: Nullable BrowserMicrophone
       , analysers :: { | analysers }
       , recorders :: O.Object (MediaRecorder -> Effect Unit)
       , buffers :: O.Object BrowserAudioBuffer
       , floatArrays :: O.Object BrowserFloatArray
       , periodicWaves :: O.Object BrowserPeriodicWave
       }
  -> Ref.Ref (Effect Unit)
  -> Ref.Ref EasingAlgorithm
  -> Ref.Ref (Scene (SceneI trigger world) assets RunAudio RunEngine Frame0 res)
  -> O.Object (AnalyserNode -> Effect (Effect Unit))
  -> NonBehavioralFFIInfo
  -> (Run res analysers -> Effect Unit)
  -> Effect Unit
runInternal audioClockStart fromEvents world' currentTimeoutCanceler currentEasingAlg currentScene analyserRefs nonBehavioralFFIInfo reporter = do
  easingAlgNow <- Ref.read currentEasingAlg
  sceneNow <- Ref.read currentScene
  audioClockPriorToComputation <- getAudioClockTime nonBehavioralFFIInfo.context
  let
    -- this is how far in the future we are telling the
    -- algorithm to calculate with respect to the audio clock
    -- it is a bet: we bet that the algorithm will finish in this amount
    -- of time
    headroom = head easingAlgNow

    headroomInSeconds = (toNumber headroom) / 1000.0

    time = (audioClockPriorToComputation - audioClockStart) + headroomInSeconds

    fromScene =
      oneFrame sceneNow
        ( coerce
            { world: fromEvents.world
            , trigger: fromEvents.trigger
            , sysTime: fromEvents.sysTime
            , time
            , headroom
            , headroomInSeconds
            }
        )
  audioClockAfterComputation <- getAudioClockTime nonBehavioralFFIInfo.context
  let
    ffi =
      FFIAudioSnapshot
        { context: nonBehavioralFFIInfo.context
        , writeHead: max audioClockAfterComputation (audioClockPriorToComputation + headroomInSeconds)
        , units: nonBehavioralFFIInfo.units
        , microphone: fromEvents.microphone
        , analysers: analyserRefs
        , recorders: fromEvents.recorders
        , buffers: fromEvents.buffers
        , floatArrays: fromEvents.floatArrays
        , periodicWaves: fromEvents.periodicWaves
        }

    applied = map (\f -> f (unit /\ ffi)) fromScene.instructions
  renderAudio (map snd applied)
  let
    remainingTimeInSeconds = audioClockPriorToComputation + headroomInSeconds - audioClockAfterComputation

    remainingTime = floor $ 1000.0 * remainingTimeInSeconds
  Ref.write (tail easingAlgNow remainingTime) currentEasingAlg
  Ref.write
    fromScene.next
    currentScene
  reporter
    { instructions: map fst applied
    , res: fromScene.res
    , remainingTimeInSeconds
    , remainingTime
    , headroom
    , headroomInSeconds
    , analysers: fromEvents.analysers
    }
  -- we thunk the world and move on to the next event
  -- note that if we did not allocate enough time, we still
  -- set a timeout of 1 so that the canceler can run in case it needs to
  canceler <-
    subscribe (sample_ world' (delay (max 1 remainingTime) (pure unit)))
      \{ world
       , sysTime
       , microphone
       , analysers
       , recorders
       , buffers
       , floatArrays
       , periodicWaves
       } ->
        runInternal audioClockStart
          { world
          , sysTime
          , trigger: Nothing
          , microphone
          , analysers
          , recorders
          , buffers
          , floatArrays
          , periodicWaves
          }
          world'
          currentTimeoutCanceler
          currentEasingAlg
          currentScene
          analyserRefs
          nonBehavioralFFIInfo
          reporter
  Ref.write canceler currentTimeoutCanceler
