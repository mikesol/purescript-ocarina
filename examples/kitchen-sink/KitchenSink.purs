module WAGS.Example.KitchenSink where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import Data.Vec ((+>), empty)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen (SubscriptionId)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Math (abs, pi)
import WAGS.Example.KitchenSink.Piece (piece)
import WAGS.Interpret (AudioContext, BrowserAudioBuffer, close, context, decodeAudioDataFromUri, getMicrophoneAndCamera, makeFloatArray, makePeriodicWave, makeUnitCache, mediaRecorderToUrl)
import WAGS.Run (Run, run)

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

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm = let fOf initialTime = mkCofree initialTime \adj -> fOf $ max 10 (initialTime - adj) in fOf 20

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

data Action
  = StartAudio
  | ReportGraph (Run String ())
  | HydrateRecording String
  | StopAudio

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
    $ [ HH.h1_
          [ HH.text "Kitchen sink" ]
      , HH.p [] [ HH.text "Unit tests for WAGS. Tries to test everything!" ]
      , HH.p [] [ HH.text "As the audio plays, the current graph should appear below." ]
      , HH.p [] [ HH.text "When data is available for a recording (which should happen every time the triangleOsc plays) an audio tag should appear." ]
      , HH.button
          [ HE.onClick \_ -> StartAudio ]
          [ HH.text "Start audio" ]
      , HH.button
          [ HE.onClick \_ -> StopAudio ]
          [ HH.text "Stop audio" ]
      , HH.p [] [ HH.text $ "Graph :: " <> show state.graph ]
      ]
    <> maybe [] (\aud -> [ HH.audio [ HP.src aud, HP.controls true ] [] ]) state.audioSrc

fetchBuffer :: ∀ (m ∷ Type -> Type). MonadAff m ⇒ AudioContext → String → m BrowserAudioBuffer
fetchBuffer audioCtx addr =
  H.liftAff $ toAffE
    $ decodeAudioDataFromUri
        audioCtx
        addr

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  HydrateRecording rec -> H.modify_ (_ { audioSrc = pure $ rec })
  ReportGraph graph -> H.modify_ (_ { graph = pure $ show graph })
  StartAudio -> do
    handleAction StopAudio
    { emitter, listener } <- H.liftEffect HS.create
    unsubscribeFromHalogen <- H.subscribe emitter
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    myWave <-
      H.liftEffect
        $ makePeriodicWave audioCtx (0.0 +> -0.1 +> empty) (0.0 +> 0.05 +> empty)
    wicked <- H.liftEffect $ makeFloatArray (makeDistortionCurve 400.0)
    let
      recorder =
        mediaRecorderToUrl
          "audio/ogg; codecs=opus"
          (HS.notify listener <<< HydrateRecording)
    { microphone } <- H.liftAff $ getMicrophoneAndCamera true false
    chimes <- fetchBuffer audioCtx "https://freesound.org/data/previews/353/353194_5121236-hq.mp3"
    shruti <- fetchBuffer audioCtx "https://freesound.org/data/previews/513/513742_153257-hq.mp3"
    reverb <- fetchBuffer audioCtx "https://freesound.org/data/previews/555/555786_10147844-hq.mp3"
    let
      ffiAudio =
        { context: audioCtx
        , writeHead: 0.0
        , units: unitCache
        , microphone: pure $ toNullable microphone
        , recorders: pure { "my-recorder": recorder }
        , worklets: {}
        , buffers:
            pure
              { "my-buffer": chimes
              , "shruti": shruti
              , "reverb": reverb
              }
        , floatArrays: pure { "my-waveshaper": wicked }
        , periodicWaves: pure { "my-wave": myWave }
        }
    unsubscribeFromWAGS <-
      H.liftEffect
        $ subscribe
            ( run
                (pure unit)
                (pure unit)
                { easingAlgorithm }
                (ffiAudio)
                piece
            )
            (HS.notify listener <<< ReportGraph)
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
