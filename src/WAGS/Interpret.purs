-- | This documents the audio units exported by FRP.Behavior.Audio.
-- | The convention for audio units is the following:
-- |
-- | - highpass :: A highpass filter
-- | - highpassT :: A highpass filter using an AudioParameter, which has a temporal offset. 
-- | - highpass_ :: A named highpass filter.  Naming audio units speeds up computation a bit and may get rid of some artifacts.
-- | - highpassT_ :: A named highpass filter with a AudioParameters.
-- |
-- | All audio units have these four varieties.  Any audio unit that is not a generator takes one or many audio units as inputs.  In addition, some audio units (like `speaker` and `gain`) have a variety with an apostrophe (`speaker'` and `gain'`) that accept a single audio unit instead of a list.
module WAGS.Interpret where

import Prelude

{-
  touchAudio
    toFFI -- we can get rid of this
    audioTime -- this we will definitely need
    instructionSet -- this is what we're providing
    ctx -- this we will definitely need
    audioInfo -- this we will definitely need
    uts -- this we will definitely need
so really, we want audioInterpreter to be:
interpretAudio :: Number -> AudioContext -> AudioInfo -> Foreign -> Instructions -> Foreign
-}
{-
import Control.Bind (bindFlipped)
import Control.Promise (Promise)
import Data.Array (catMaybes, head, takeEnd, (!!))
import Data.Array as A
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Int (round, toNumber)
import Data.JSDate (getTime, now)
import Data.Lens (_1, over, traversed)
import Data.List (List(..), fromFoldable, partition, (:))
import Data.List as DL
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Set (Set, member)
import Data.Set as DS
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split, take)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class LtEq, class Pos, D1, D2, D3, D4, D5, D20, toInt')
import Data.Vec (Vec)
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (Aff, error, joinFiber, launchAff, launchAff_, throwError)
import Effect.Class.Console (warn)
import Effect.Class.Console as Log
import Effect.Exception (try)
import Effect.Ref (modify, new, read, write)
import FRP.Behavior (ABehavior, Behavior, sample_)
import FRP.Event (Event, EventIO, create, subscribe)
import FRP.Event.Time (interval)
import Foreign (Foreign)
import Foreign.Object (Object, filterWithKey)
import Foreign.Object as O
import Prim.Boolean (False, True)
import Prim.Row (class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Symbol (class Compare)
import Prim.TypeError (class Fail, Text)
import Record (merge)
import Record.Unsafe (unsafeGet)
import Type.Data.Boolean (class And, class Not)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Graph.Parameter (AudioParameterTransition)
import WAGS.Rendered (AnAudioUnit(..), Instruction)
import Web.DOM.Document (createElement)
import Web.HTML (HTMLCanvasElement, HTMLImageElement, HTMLVideoElement, window)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLVideoElement (videoHeight, videoWidth)
import Web.HTML.Window (document)

foreign import data MediaRecorder :: Type

foreign import data BrowserPeriodicWave :: Type

foreign import data BrowserAudioBuffer :: Type

foreign import data BrowserFloatArray :: Type

foreign import data BrowserAudioTrack :: Type

foreign import data AudioContext :: Type
foreign import data BrowserMicrophone :: Type

foreign import stopMediaRecorder :: MediaRecorder -> Effect Unit

foreign import mediaRecorderToUrl :: String -> (String -> Effect Unit) -> MediaRecorder -> Effect Unit

foreign import isTypeSupported :: String -> Effect Boolean

foreign import decodeAudioDataFromUri :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

foreign import decodeAudioDataFromBase64EncodedString :: AudioContext -> String -> Effect (Promise BrowserAudioBuffer)

foreign import audioWorkletAddModule :: AudioContext -> String -> Effect (Promise Unit)

foreign import makeAudioContext :: Effect AudioContext

foreign import makePeriodicWaveImpl :: AudioContext -> Array Number -> Array Number -> Effect BrowserPeriodicWave

foreign import makeAudioTrack :: String -> Effect BrowserAudioTrack

foreign import makeAudioBuffer :: AudioContext -> AudioBuffer -> Effect BrowserAudioBuffer

foreign import makeFloatArray :: Array Number -> Effect BrowserFloatArray

makePeriodicWave ::
  forall len.
  Pos len =>
  AudioContext ->
  Vec len Number ->
  Vec len Number ->
  Effect BrowserPeriodicWave
makePeriodicWave ctx a b = makePeriodicWaveImpl ctx (V.toArray a) (V.toArray b)

audioBuffer ::
  forall bch blen.
  Pos bch =>
  Pos blen =>
  Int ->
  Vec bch (Vec blen Number) ->
  AudioBuffer
audioBuffer i v = AudioBuffer i (map V.toArray $ V.toArray v)
data AudioBuffer
  = AudioBuffer Int (Array (Array Number))

type FFIPredicates
  = { justly :: forall a. a -> Maybe a
    , tupply :: forall a b. a -> b -> Tuple a b
    , isNothing :: forall a. Maybe a -> Boolean
    , isNoRamp :: AudioParameterTransition -> Boolean
    , isImmediately :: AudioParameterTransition -> Boolean
    , isLinearRamp :: AudioParameterTransition -> Boolean
    , isExponentialRamp :: AudioParameterTransition -> Boolean
    , isMicrophone :: (AnAudioUnit -> Boolean)
    , isPlayBuf :: (AnAudioUnit -> Boolean)
    , isLoopBuf :: (AnAudioUnit -> Boolean)
    , isLowpass :: (AnAudioUnit -> Boolean)
    , isHighpass :: (AnAudioUnit -> Boolean)
    , isBandpass :: (AnAudioUnit -> Boolean)
    , isLowshelf :: (AnAudioUnit -> Boolean)
    , isHighshelf :: (AnAudioUnit -> Boolean)
    , isPeaking :: (AnAudioUnit -> Boolean)
    , isNotch :: (AnAudioUnit -> Boolean)
    , isAllpass :: (AnAudioUnit -> Boolean)
    , isConvolver :: (AnAudioUnit -> Boolean)
    , isDynamicsCompressor :: (AnAudioUnit -> Boolean)
    , isSawtoothOsc :: (AnAudioUnit -> Boolean)
    , isTriangleOsc :: (AnAudioUnit -> Boolean)
    , isPeriodicOsc :: (AnAudioUnit -> Boolean)
    , isWaveShaper :: (AnAudioUnit -> Boolean)
    , isSinOsc :: (AnAudioUnit -> Boolean)
    , isSquareOsc :: (AnAudioUnit -> Boolean)
    , isStereoPanner :: (AnAudioUnit -> Boolean)
    , isConstant :: (AnAudioUnit -> Boolean)
    , isDelay :: (AnAudioUnit -> Boolean)
    , isGain :: (AnAudioUnit -> Boolean)
    , isSpeaker :: (AnAudioUnit -> Boolean)
    , isRecorder :: (AnAudioUnit -> Boolean)
    , isNoSound :: (AnAudioUnit -> Boolean)
    , isSplitRes :: (AnAudioUnit -> Boolean)
    , isDupRes :: (AnAudioUnit -> Boolean)
    , isStop :: (Instruction -> Boolean)
    , isFree :: (Instruction -> Boolean)
    , isDisconnectFrom :: (Instruction -> Boolean)
    , isConnectTo :: (Instruction -> Boolean)
    , isShuffle :: (Instruction -> Boolean)
    , isNewUnit :: (Instruction -> Boolean)
    , isSetFrequency :: (Instruction -> Boolean)
    , isSetThreshold :: (Instruction -> Boolean)
    , isSetKnee :: (Instruction -> Boolean)
    , isSetRatio :: (Instruction -> Boolean)
    , isSetAttack :: (Instruction -> Boolean)
    , isSetRelease :: (Instruction -> Boolean)
    , isSetBuffer :: (Instruction -> Boolean)
    , isSetQ :: (Instruction -> Boolean)
    , isSetPlaybackRate :: (Instruction -> Boolean)
    , isSetPeriodicWave :: (Instruction -> Boolean)
    , isSetCurve :: (Instruction -> Boolean)
    , isSetOversample :: (Instruction -> Boolean)
    , isSetLoopStart :: (Instruction -> Boolean)
    , isSetLoopEnd :: (Instruction -> Boolean)
    , isSetPan :: (Instruction -> Boolean)
    , isSetGain :: (Instruction -> Boolean)
    , isSetDelay :: (Instruction -> Boolean)
    , isSetOffset :: (Instruction -> Boolean)
    }

toFFI =
  { justly: Just
  , tupply: Tuple
  , isNothing: isNothing
  , isNoRamp: isNoRamp_
  , isImmediately: isImmediately_
  , isLinearRamp: isLinearRamp_
  , isExponentialRamp: isExponentialRamp_
  , isMicrophone: isMicrophone_
  , isAudioWorkletGenerator: isAudioWorkletGenerator_
  , isAudioWorkletProcessor: isAudioWorkletProcessor_
  , isAudioWorkletAggregator: isAudioWorkletAggregator_
  , isPlay: isPlay_
  , isPlayBuf: isPlayBuf_
  , isLoopBuf: isLoopBuf_
  , isIIRFilter: isIIRFilter_
  , isLowpass: isLowpass_
  , isHighpass: isHighpass_
  , isBandpass: isBandpass_
  , isLowshelf: isLowshelf_
  , isHighshelf: isHighshelf_
  , isPeaking: isPeaking_
  , isNotch: isNotch_
  , isAllpass: isAllpass_
  , isConvolver: isConvolver_
  , isDynamicsCompressor: isDynamicsCompressor_
  , isSawtoothOsc: isSawtoothOsc_
  , isTriangleOsc: isTriangleOsc_
  , isPeriodicOsc: isPeriodicOsc_
  , isWaveShaper: isWaveShaper_
  , isDup: isDup_
  , isSinOsc: isSinOsc_
  , isSquareOsc: isSquareOsc_
  , isSplitter: isSplitter_
  , isStereoPanner: isStereoPanner_
  , isPanner: isPanner_
  , isMul: isMul_
  , isAdd: isAdd_
  , isSwap: isSwap_
  , isMerger: isMerger_
  , isConstant: isConstant_
  , isDelay: isDelay_
  , isGain: isGain_
  , isSpeaker: isSpeaker_
  , isRecorder: isRecorder_
  , isNoSound: isNoSound_
  , isSplitRes: isSplitRes_
  , isDupRes: isDupRes_
  , isStop: isStop_
  , isFree: isFree_
  , isDisconnectFrom: isDisconnectFrom_
  , isConnectTo: isConnectTo_
  , isShuffle: isShuffle_
  , isNewUnit: isNewUnit_
  , isSetFrequency: isSetFrequency_
  , isSetThreshold: isSetThreshold_
  , isSetKnee: isSetKnee_
  , isSetRatio: isSetRatio_
  , isSetAttack: isSetAttack_
  , isSetRelease: isSetRelease_
  , isSetBuffer: isSetBuffer_
  , isSetQ: isSetQ_
  , isSetPlaybackRate: isSetPlaybackRate_
  , isSetPeriodicWave: isSetPeriodicWave_
  , isSetCurve: isSetCurve_
  , isSetOversample: isSetOversample_
  , isSetLoopStart: isSetLoopStart_
  , isSetLoopEnd: isSetLoopEnd_
  , isSetPan: isSetPan_
  , isSetGain: isSetGain_
  , isSetDelay: isSetDelay_
  , isSetOffset: isSetOffset_
  , isSetCustomParam: isSetCustomParam_
  , isSetConeInnerAngle: isSetConeInnerAngle_
  , isSetConeOuterAngle: isSetConeOuterAngle_
  , isSetConeOuterGain: isSetConeOuterGain_
  , isSetDistanceModel: isSetDistanceModel_
  , isSetMaxDistance: isSetMaxDistance_
  , isSetOrientationX: isSetOrientationX_
  , isSetOrientationY: isSetOrientationY_
  , isSetOrientationZ: isSetOrientationZ_
  , isSetPanningModel: isSetPanningModel_
  , isSetPositionX: isSetPositionX_
  , isSetPositionY: isSetPositionY_
  , isSetPositionZ: isSetPositionZ_
  , isSetRefDistance: isSetRefDistance_
  , isSetRolloffFactor: isSetRolloffFactor_
  } ::
    FFIPredicates

type RecorderSignature recorder
  = recorder -> Effect Unit

type RecorderInfo dataavailableEvent errorEvent pauseEvent resumeEvent startEvent stopEvent
  = { ondataavailable :: dataavailableEvent -> Effect Unit
    , onerror :: errorEvent -> Effect Unit
    , onpause :: pauseEvent -> Effect Unit
    , onresume :: resumeEvent -> Effect Unit
    , onstart :: startEvent -> Effect Unit
    , onstop :: stopEvent -> Effect Unit
    }

type AudioInfo = 
    AudioInfo' (Object BrowserMicrophone) (Object (RecorderSignature MediaRecorder)) (Object BrowserAudioTrack) (Object BrowserAudioBuffer) (Object BrowserFloatArray) (Object BrowserPeriodicWave)

type AudioInfo' microphones recorders tracks buffers floatArrays periodicWaves
  = { microphones :: microphones
    , recorders :: recorders
    , tracks :: tracks
    , buffers :: buffers
    , floatArrays :: floatArrays
    , periodicWaves :: periodicWaves
    }

-- the reason canvas elements are effectful is because,
-- unlike audio elements in html,
-- canvases can be killed off based on a rendering function, ie in halogen
-- we want to be able to throw if the canvas does not exist
type VisualInfo accumulator
  = { canvases :: Object (Effect CanvasElement)
    , images :: Object HTMLImageElement
    , videos :: Object HTMLVideoElement
    , cameras ::
        Object
          { camera :: HTMLVideoElement
          , cache :: accumulator -> Number -> List (Tuple Number HTMLCanvasElement) -> List (Tuple Number HTMLCanvasElement)
          }
    , sourceCanvases :: Object HTMLCanvasElement
    }

foreign import getAudioClockTime :: AudioContext -> Effect Number

type CanvasInfo'
  = { w :: Number, h :: Number, boundingClientRect :: Rectangle }

newtype CanvasInfo
  = CanvasInfo CanvasInfo'

dummyCanvasInfo :: CanvasInfo'
dummyCanvasInfo = { w: 0.0, h: 0.0, boundingClientRect: { width: 0.0, height: 0.0, x: 0.0, y: 0.0 } }

type Run accumulator env
  = accumulator ->
    AudioContext ->
    EngineInfo ->
    AudioInfo ->
    VisualInfo accumulator ->
    Exporter env accumulator ->
    Effect (Effect Unit)

type RunInBrowser callback accumulator env
  = callback -> Run accumulator env

type RunInBrowser_ callback accumulator env
  = Effect callback -> Run accumulator env

type Exporter env accumulator
  = { acquire :: Aff env
    , use :: env -> (BuildingBlocks accumulator) -> Aff Unit
    , release :: env -> Aff Unit
    }

defaultExporter :: forall accumulator. Exporter Unit accumulator
defaultExporter =
  { acquire: pure unit
  , use: \_ _ -> pure unit
  , release: \_ -> pure unit
  }

type EngineInfo
  = { msBetweenSamples :: Int
    , msBetweenPings :: Int
    , fastforwardLowerBound :: Number
    , rewindUpperBound :: Number
    , initialOffset :: Number
    , doWebAudio :: Boolean
    }

type AnimationInfo
  = { painting ::
        { words :: M.Map MeasurableText TextMetrics
        , webcamInfo ::
            Maybe
              { width :: Int
              , height :: Int
              }
        , audioClockRelativePosition :: Number
        } ->
        Painting
    , words :: List MeasurableText
    }

newtype AV ch accumulator
  = AV
  { audio :: Maybe (AudioUnit ch)
  , visual :: Maybe AnimationInfo
  , accumulator :: accumulator
  }

type BuildingBlocks accumulator
  = { id :: Int
    , timeStamp :: Number
    , audio :: Maybe (Array Instruction)
    , canvas :: Maybe Painting
    , accumulator :: accumulator
    }

data Animation
  = Animation AnimationInfo

data IAnimation accumulator
  = IAnimation AnimationInfo accumulator

data IAudioUnit ch accumulator
  = IAudioUnit (AudioUnit ch) accumulator

getFirstCanvas :: Object (Effect CanvasElement) -> Maybe (Effect CanvasElement)
getFirstCanvas = map snd <<< A.head <<< O.toUnfoldable


htmlCanvasElementToCanvasElement :: HTMLCanvasElement -> CanvasElement
htmlCanvasElementToCanvasElement = unsafeCoerce

webcamToCanvas :: { width :: Int, height :: Int } -> CanvasImageSource -> Effect HTMLCanvasElement
webcamToCanvas { width, height } src = do
  document <- (toDocument <$> (document =<< window))
  node <- HTMLCanvasElement.fromElement <$> (createElement "canvas" document)
  canvas <- case node of
    Nothing -> throwError (error "Could not convert to canvas node")
    Just x -> pure x
  HTMLCanvasElement.setWidth width canvas
  HTMLCanvasElement.setHeight height canvas
  canvasCtx <- getContext2D $ htmlCanvasElementToCanvasElement canvas
  drawImage canvasCtx src 0.0 0.0
  pure canvas

renderPainting :: Context2D -> ImageSources -> CanvasInfo' -> Painting -> Effect Unit
renderPainting canvasCtx imageSources canvasInfo painting = do
  res <-
    try do
      clearRect canvasCtx
        { height: canvasInfo.h
        , width: canvasInfo.w
        , x: 0.0
        , y: 0.0
        }
      render canvasCtx imageSources painting
  either (warn <<< show) pure res

runInBrowser :: forall callback accumulator env.
  callback ->
  accumulator ->
  AudioContext ->
  EngineInfo ->
  AudioInfo ->
  VisualInfo accumulator ->
  Exporter env accumulator ->
  Effect (Effect Unit)
runInBrowser scene accumulator ctx engineInfo audioInfo visualInfo exporter = do
    let
      __contract = toNumber $ engineInfo.msBetweenSamples
    __accumulator <- new accumulator
    __totalFromStart <- new 0.0
    ciRef <- new 0
    __exporterQueueRef <- (launchAff $ pure unit) >>= new
    __totalTillProgram <- new 0.0
    __totalProgram <- new 0.0
    __totalPostProgram <- new 0.0
    __webcamCache <- new (Nil :: List (Tuple Number HTMLCanvasElement))
    let
      webcam = (map snd <<< head <<< O.toUnfoldable) visualInfo.cameras
    reconRef <-
      new
        { grouped: M.empty
        , flat: M.empty
        }
    let
      tOffset = engineInfo.initialOffset
    clock <- new 0
    units <- new ({ generators: [], recorders: [] } :: TouchAudioIO)
    audioClockStart <- getAudioClockTime ctx
    clockClockStart <- map ((_ / 1000.0) <<< getTime) now
    fiber <- launchAff exporter.acquire
    bam <-
      subscribe
        (interval engineInfo.msBetweenPings)
        ( const do
            ct <- read clock
            write (ct + engineInfo.msBetweenSamples) clock
            acc_ <- getAudioClockTime ctx
            curIt <- read ciRef
            write (curIt + 1) ciRef
            clockNow_ <- read clock
            let
              startingPosWRT =
                ( ((toNumber clockNow_ + tOffset) / 1000.0)
                    - (acc_ - audioClockStart)
                )
            if (startingPosWRT > engineInfo.rewindUpperBound) then
              -- reset the clock
              ( do
                  let
                    newV = (clockNow_ - engineInfo.msBetweenSamples)
                  -- log $ "Rewinding " <> show clockNow_ <> " " <> show newV <> " " <> show startingPosWRT
                  write newV clock
              )
            else do
              if (startingPosWRT < engineInfo.fastforwardLowerBound) then
                ( do
                    let
                      newV = clockNow_ + engineInfo.msBetweenSamples
                    warn $ "Fastforwarding " <> show clockNow_ <> " " <> show newV <> " " <> show startingPosWRT
                    write newV clock
                )
              else
                pure unit
              __startTime <- map getTime now
              _accNow <- read __accumulator
              let
                __cvsNow = getFirstCanvas visualInfo.canvases
              canvasInfo <-
                maybe (pure dummyCanvasInfo)
                  ( \_cvsNow -> do
                      __r <-
                        try do
                          __cvs <- _cvsNow
                          w <- getCanvasWidth __cvs
                          h <- getCanvasHeight __cvs
                          boundingClientRect <- getBoundingClientRect __cvs
                          pure $ { w, h, boundingClientRect }
                      either (const $ pure dummyCanvasInfo) pure __r
                  )
                  __cvsNow
              let
                timeInSeconds = toNumber ct / 1000.0

                behavior = scene _accNow (CanvasInfo canvasInfo) timeInSeconds
              bang <- create :: Effect (EventIO Unit)
              let
                behaviorSampled = sample_ behavior bang.event
              unsub <-
                subscribe behaviorSampled
                  ( \(AV { audio: ava, visual: avv, accumulator: avz }) -> do
                      let
                        audioClockOffset = (toNumber clockNow_ + tOffset) / 1000.0

                        audioTime = audioClockStart + audioClockOffset
                      write avz __accumulator
                      painting <- case avv of
                        Nothing -> pure Nothing
                        Just x -> do
                          let
                            cvs_ = getFirstCanvas visualInfo.canvases
                          case cvs_ of
                            Nothing -> pure Nothing
                            Just cvs__ -> do
                              cvs <- cvs__
                              canvasCtx <- getContext2D cvs
                              words <- measurableTextToMetrics canvasCtx x.words
                              __renderTime <- map ((_ / 1000.0) <<< getTime) now
                              webcamInfo <- case webcam of
                                Nothing -> pure Nothing
                                Just v -> do
                                  width <- videoWidth v.camera
                                  height <- videoHeight v.camera
                                  pure $ Just { width, height, camera: v.camera, cache: v.cache }
                              let
                                currentPainting =
                                  x.painting
                                    { words
                                    , webcamInfo: (\{ width, height } -> { width, height }) <$> webcamInfo
                                    , audioClockRelativePosition: audioClockOffset - (__renderTime - clockClockStart)
                                    }
                              -- webcam
                              case webcamInfo of
                                Nothing -> pure unit
                                Just { width, height, camera, cache } -> do
                                  canvas <- webcamToCanvas { width, height } (htmlVideoElemntToImageSource camera)
                                  void $ modify (cache avz __renderTime <<< Cons (Tuple __renderTime canvas)) __webcamCache
                              webcamCache <- over (traversed <<< _1) (__renderTime - _) <$> read __webcamCache
                              -- end webcam
                              renderPainting canvasCtx
                                { images: visualInfo.images
                                , videos: visualInfo.videos
                                , canvases: visualInfo.sourceCanvases
                                , webcam: webcamCache
                                }
                                canvasInfo
                                currentPainting
                              pure $ Just currentPainting
                      maybe (pure unit)
                        ( \aud -> do
                            -- __t0 <- map getTime now
                            let
                              i = audioToPtr aud
                            -- __t1 <- map getTime now
                            let
                              cur = { flat: i.flat, grouped: audioGrouper (DL.fromFoldable i.flat) }
                            prev <- read reconRef
                            write cur reconRef
                            --__t2 <- map getTime now
                            let debug = false
                            let
                              instructionSet = reconciliationToInstructionSet prev cur
                            --__t3 <- map getTime now
                            case debug of
                              true -> do
                                Log.info "prev"
                                Log.info (show prev)
                                Log.info "cur"
                                Log.info (show cur)
                                Log.info "instructionSet"
                                Log.info (show instructionSet)
                              false -> pure unit
                            exporterQueueRef <- read __exporterQueueRef
                            launchAff
                              ( do
                                  env <- joinFiber fiber
                                  _ <- joinFiber exporterQueueRef
                                  exporter.use
                                    env
                                    { id: curIt
                                    , accumulator: accumulator
                                    , timeStamp: timeInSeconds
                                    , audio: (ava >>= (const $ Just instructionSet))
                                    , canvas: painting
                                    }
                              )
                              >>= flip write __exporterQueueRef
                            uts <- read units
                            uts' <-
                              if engineInfo.doWebAudio then
                                touchAudio
                                  toFFI
                                  audioTime
                                  instructionSet
                                  ctx
                                  audioInfo
                                  uts
                              else
                                pure { generators: [], recorders: [] }
                            write uts' units
                            __endTime <- map getTime now
                            if (__endTime - __startTime) >= __contract then
                              warn
                                ( "Audio control processing is too slow. It took this long: "
                                    <> show (__endTime - __startTime)
                                    <> " but it needs to take this long: "
                                    <> show __contract
                                )
                            else
                              pure unit
                            --log $ "stats :: " <> (show $ __t0 - __startTime) <> " @ " <> (show $ __t1 - __t0) <> " @ " <> (show $ __t2 - __t1) <> " @ " <> (show $ __t3 - __t2) <> " @ " <> (show $ __endTime - __t3) <> " @ " <> (show $ __endTime - __startTime)
                            pure unit
                        )
                        ava
                  )
              bang.push unit
              unsub
        )
    pure
      ( do
          bam
          uts <- read units
          traverse_ stopMediaRecorder uts.recorders
          launchAff_
            ( do
                env <- joinFiber fiber
                exporter.release env
            )
      )

-- | The main executor loop in the browser
-- | Accepts an effectful scene
runInBrowser_ ::
  forall accumulator callback env.
  RunnableMedia callback accumulator env =>
  Effect callback ->
  accumulator ->
  AudioContext ->
  EngineInfo ->
  AudioInfo ->
  VisualInfo accumulator ->
  Exporter env accumulator ->
  Effect (Effect Unit)
runInBrowser_ scene' accumulator ctx engineInfo audioInfo visualInfo exporter = do
  scene <- scene'
  runInBrowser scene accumulator ctx engineInfo audioInfo visualInfo exporter
-}