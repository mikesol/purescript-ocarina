module WAGS.Example.Failure where

import Prelude
import Control.Applicative.Indexed (ipure, (:*>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Foreign.Object as O
import Halogen (SubscriptionId)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Math ((%), pi)
import WAGS.Change (ichange)
import WAGS.Control.Functions (icont)
import WAGS.Control.Functions.Validated (ibranch, (@!>))
import WAGS.Control.Types (Frame0, Scene, WAG)
import WAGS.Example.KitchenSink (fetchBuffer)
import WAGS.Graph.AudioUnit (OnOff(..), OversampleTwoX, TConvolver, TPeriodicOsc, TPlayBuf, TRecorder, TSpeaker, TWaveShaper)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, makeFloatArray, makePeriodicWave, makeUnitCache, mediaRecorderToUrl)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, SceneI, RunEngine, run)

type ShouldFail
  = { speaker :: TSpeaker /\ { badWshape :: Unit, badPosc :: Unit }
    , badWshape :: TWaveShaper "fail" OversampleTwoX /\ { badConv :: Unit }
    , badConv :: TConvolver "fail" /\ { badRec :: Unit }
    , badRec :: TRecorder "fail" /\ { badBuf :: Unit }
    , badBuf :: TPlayBuf /\ {}
    , badPosc :: TPeriodicOsc /\ {}
    }

type ShouldSucceed
  = { speaker :: TSpeaker /\ { wshape :: Unit, posc :: Unit }
    , wshape :: TWaveShaper "success" OversampleTwoX /\ { conv :: Unit }
    , conv :: TConvolver "success" /\ { rec :: Unit }
    , rec :: TRecorder "success" /\ { buf :: Unit }
    , buf :: TPlayBuf /\ {}
    , posc :: TPeriodicOsc /\ {}
    }

shouldFail :: forall proof. WAG RunAudio RunEngine proof Unit ShouldFail Unit -> Scene (SceneI Unit Unit) RunAudio RunEngine proof Unit
shouldFail =
  ibranch \e a ->
    if e.time % 4.0 < 2.0 then
      Right $ ipure unit
    else
      Left
        $ icont shouldSucceed
            ( ipatch
                :*> ichange
                    { posc: { waveform: "myWavetable", onOff: On }
                    , buf: { buffer: "myBuffer", onOff: On }
                    }
            )

shouldSucceed :: forall proof. WAG RunAudio RunEngine proof Unit ShouldSucceed Unit -> Scene (SceneI Unit Unit) RunAudio RunEngine proof Unit
shouldSucceed =
  ibranch \e a ->
    if e.time % 4.0 > 2.0 then
      Right $ ipure unit
    else
      Left
        $ icont shouldFail
            ( ipatch
                :*> ichange
                    { badPosc: { waveform: "?!?#$@#$", onOff: On }
                    , badBuf: { buffer: "sjdgfkfbg", onOff: On }
                    }
            )

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece =
  ( const
      ( ipatch
          :*> ichange
              { badPosc: { waveform: "?!?#$@#$", onOff: On }
              , badBuf: { buffer: "sjdgfkfbg", onOff: On }
              }
      )
  )
    @!> shouldFail

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State
  = { unsubscribeFromWAGS :: Effect Unit
    , unsubscribeFromHalogen :: Maybe SubscriptionId
    , audioCtx :: Maybe AudioContext
    , graph :: Maybe String
    , audioSrc :: Maybe String
    }

makeDistortionCurve :: Number -> Array Number
makeDistortionCurve k =
  map
    ( \i ->
        let
          x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
        in
          (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
    )
    (0 .. (n_samples - 1))
  where
  n_samples = 44100

  deg = pi / 180.0

data Action
  = StartAudio
  | StopAudio
  | HydrateRecording String

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribeFromWAGS: pure unit
  , unsubscribeFromHalogen: Nothing
  , audioCtx: Nothing
  , graph: Nothing
  , audioSrc: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [ HH.h1_
        [ HH.text "Resilient failure" ]
    , HH.p_ [ HH.text "Check the logs to see failure handled (sort of) gracefully." ]
    , HH.button
        [ HE.onClick \_ -> StartAudio ]
        [ HH.text "Start audio" ]
    , HH.button
        [ HE.onClick \_ -> StopAudio ]
        [ HH.text "Stop audio" ]
    ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  HydrateRecording rec -> H.modify_ (_ { audioSrc = pure $ rec })
  StartAudio -> do
    handleAction StopAudio
    { emitter, listener } <- H.liftEffect HS.create
    unsubscribeFromHalogen <- H.subscribe emitter
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    myWave <-
      H.liftEffect
        $ makePeriodicWave audioCtx (0.0 +> -0.1 +> V.empty) (0.0 +> 0.05 +> V.empty)
    wicked <- H.liftEffect $ makeFloatArray (makeDistortionCurve 400.0)
    let
      recorder =
        mediaRecorderToUrl
          "audio/ogg; codecs=opus"
          (HS.notify listener <<< HydrateRecording)
    myBuffer <- fetchBuffer audioCtx "https://freesound.org/data/previews/353/353194_5121236-hq.mp3"
    success <- fetchBuffer audioCtx "https://freesound.org/data/previews/555/555786_10147844-hq.mp3"
    let
      ffiAudio =
        (defaultFFIAudio audioCtx unitCache)
          { periodicWaves = O.fromFoldable [ "myWavetable" /\ myWave ]
          , buffers =
            O.fromFoldable
              [ "myBuffer" /\ myBuffer
              , "success" /\ success
              ]
          , floatArrays = O.singleton "success" wicked
          , recorders = O.singleton "success" recorder
          }
    unsubscribeFromWAGS <-
      H.liftEffect
        $ subscribe
            ( run
                (pure unit)
                (pure unit)
                { easingAlgorithm }
                (FFIAudio ffiAudio)
                piece
            )
            (const $ pure unit)
    H.modify_
      _
        { unsubscribeFromWAGS = unsubscribeFromWAGS
        , unsubscribeFromHalogen = pure unsubscribeFromHalogen
        , audioCtx = Just audioCtx
        }
  StopAudio -> do
    { unsubscribeFromWAGS, unsubscribeFromHalogen, audioCtx } <- H.get
    H.liftEffect unsubscribeFromWAGS
    for_ audioCtx (H.liftEffect <<< close)
    for_ unsubscribeFromHalogen H.unsubscribe
    H.modify_ _ { unsubscribeFromWAGS = pure unit, audioCtx = Nothing }
