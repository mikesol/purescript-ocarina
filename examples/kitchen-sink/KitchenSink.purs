module WAGS.Example.KitchenSink where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Array ((..))
import Data.Vec ((+>), empty)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Foreign.Object as O
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math (abs, pi)
import WAGS.Example.KitchenSink.TLP (piece)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, decodeAudioDataFromUri, defaultFFIAudio, makeFloatArray, makePeriodicWave, makeUnitCache)
import WAGS.Run (run)

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
  = { unsubscribe :: Effect Unit
    , audioCtx :: Maybe AudioContext
    }

data Action
  = StartAudio
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
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [ HH.h1_
        [ HH.text "Kitchen sink" ]
    , HH.p [] [ HH.text "Unit tests for WAGS. Tries to test everything!" ]
    , HH.button
        [ HE.onClick \_ -> StartAudio ]
        [ HH.text "Start audio" ]
    , HH.button
        [ HE.onClick \_ -> StopAudio ]
        [ HH.text "Stop audio" ]
    ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    myWave <-
      H.liftEffect
        $ makePeriodicWave audioCtx (0.3 +> -0.1 +> empty) (-0.25 +> 0.05 +> empty)
    wicked <- H.liftEffect $ makeFloatArray (makeDistortionCurve 400.0)
    chimes <-
      H.liftAff $ toAffE
        $ decodeAudioDataFromUri
            audioCtx
            "https://freesound.org/data/previews/353/353194_5121236-hq.mp3"
    let
      ffiAudio =
        (defaultFFIAudio audioCtx unitCache)
          { periodicWaves = O.singleton "my-wave" myWave
          , buffers = O.singleton "my-buffer" chimes
          , floatArrays = O.singleton "my-waveshaper" wicked
          }
    unsubscribe <-
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
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
