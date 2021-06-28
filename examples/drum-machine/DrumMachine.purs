module WAGS.Example.DrumMachine where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import FRP.Event (subscribe)
import Foreign.Object as O
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CGain, CSpeaker, CPlayBuf, gain, speaker, playBuf)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TLoopBuf, TSpeaker)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Run (RunAudio, SceneI, RunEngine, run)

type SceneTemplate
  = CSpeaker
      { gain0 :: CGain { play0 :: CPlayBuf }
      }

type SceneType
  = { speaker :: TSpeaker /\ { gain0 :: Unit }
    , gain0 :: TGain /\ { play0 :: Unit }
    , play0 :: TLoopBuf /\ {}
    }

gap = 0.27 :: Number

scene :: Boolean -> SceneI Unit Unit -> SceneTemplate
scene shouldReset { time } =
  let
    tgFloor = floor (time / gap)
  in
    speaker
      { gain0:
          gain 1.0
            { play0:
                playBuf
                  { onOff:
                      if (not shouldReset) then
                        pure On
                      else
                        ff ((toNumber (tgFloor + 1) * gap) - time) (pure OffOn)
                  }
                  "snare"
            }
      }

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece =
  (\e -> icreate (scene false e) $> 0.0)
    @!> iloop \e lastCrossing ->
        let
          tgFloor = floor (e.time / gap)

          crossingDivide = tgFloor /= floor ((e.time + 0.06) / gap)
          crossDiff = e.time - lastCrossing
          shouldReset = crossingDivide && crossDiff > 0.2
        in
          ichange (scene shouldReset e) $> (if shouldReset then e.time else lastCrossing)

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
        [ HH.text "Drum Machine" ]
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
    snare <-
      H.liftAff $ toAffE
        $ decodeAudioDataFromUri
            audioCtx
            "https://freesound.org/data/previews/270/270156_1125482-hq.mp3"
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache) { buffers = O.singleton "snare" snare }
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) (pure unit) { easingAlgorithm } (FFIAudio ffiAudio) piece)
            (Log.info <<< show)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
