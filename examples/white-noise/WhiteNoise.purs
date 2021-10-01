module WAGS.Example.WhiteNoise where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJavascript)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D0, D1, d0, d1)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Math (sin, pi)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CAudioWorkletNode, CSpeaker, audioWorkletNode, speaker)
import WAGS.Graph.AudioUnit (AudioWorkletNode, AudioWorkletNodeOptions(..), TAudioWorkletNode, TSpeaker)
import WAGS.Graph.Parameter (AudioParameter, ff)
import WAGS.Graph.Worklet (AudioWorkletNodeRequest(..), AudioWorkletNodeResponse)
import WAGS.Interpret (audioWorkletAddModule, close, context, defaultFFIAudio, makeUnitCache)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.WebAPI (AudioContext)
import Web.File.Blob as Blob
import Web.File.Url (createObjectURL)

type WhiteNoise = AudioWorkletNode "white-noise-processor"
  D0
  D1
  D1
  (customGain :: AudioParameter)
  ()

type WhiteNoiseResponse = AudioWorkletNodeResponse "white-noise-processor"
  D0
  D1
  D1
  (customGain :: AudioParameter)
  ()

type WhiteNoiseRequest = AudioWorkletNodeRequest "white-noise-processor"
  D0
  D1
  D1
  (customGain :: AudioParameter)
  ()

type World = { noiseUnit :: WhiteNoiseResponse }

type SceneTemplate
  = CSpeaker
  { whiteNoise :: CAudioWorkletNode "white-noise-processor" D0 D1 D1 (customGain :: AudioParameter) () {}
  }

type SceneType
  =
  ( speaker :: TSpeaker /\ { whiteNoise :: Unit }
  , whiteNoise :: TAudioWorkletNode "white-noise-processor" D0 D1 D1 (customGain :: AudioParameter) () /\ {}
  )

scene :: WhiteNoiseResponse -> Number -> SceneTemplate
scene wnr time =
  speaker
    { whiteNoise:
        audioWorkletNode wnr
          ( AudioWorkletNodeOptions
              { numberOfInputs: d0
              , numberOfOutputs: d1
              , outputChannelCount: d1
              , parameterData: { customGain: ff 0.06 $ pure $ 0.02 + sin (pi * time) * 0.01 }
              , processorOptions: {}
              }
          )
          {}
    }

createFrame :: SceneI Unit World () -> IxWAG RunAudio RunEngine Frame0 Unit () SceneType Unit
createFrame (SceneI { time, world: { noiseUnit } }) = icreate (scene noiseUnit time)

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit
piece = createFrame @!> iloop \(SceneI { time, world: { noiseUnit } }) _ -> ichange (scene noiseUnit time)

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
  =
  { unsubscribe :: Effect Unit
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

buttonStyle :: forall r i. HP.IProp (style :: String | r) i
buttonStyle = HP.style "padding: 3px; margin: 5px"

render :: forall m. State -> H.ComponentHTML Action () m
render =
  const do
    HH.div_
      [ HH.h1_
          [ HH.text "n0iseeeeeeeee" ]
      , HH.button
          [ HE.onClick \_ -> StartAudio, buttonStyle ]
          [ HH.text "Play" ]
      , HH.button
          [ HE.onClick \_ -> StopAudio, buttonStyle ]
          [ HH.text "Stop" ]
      ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    handleAction StopAudio
    { unsubscribe, audioCtx } <- do
      audioCtx <- H.liftEffect context
      objUrl <- H.liftEffect $ createObjectURL
        ( Blob.fromArray
            [ """// white-noise-processor.js
class WhiteNoiseProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors () {
    return [{
      name: 'customGain',
      defaultValue: 1,
      minValue: 0,
      maxValue: 1,
      automationRate: 'a-rate'
    }]
  }

  process (inputs, outputs, parameters) {
    const output = outputs[0]
    output.forEach(channel => {
      for (let i = 0; i < channel.length; i++) {
        channel[i] = (Math.random() * 2 - 1) *
          (parameters['customGain'].length > 1 ? parameters['customGain'][i] : parameters['customGain'][0])
        // note: a parameter contains an array of 128 values (one value for each of 128 samples),
        // however it may contain a single value which is to be used for all 128 samples
        // if no automation is scheduled for the moment.
      }
    })
    return true
  }
}

registerProcessor('white-noise-processor', WhiteNoiseProcessor)"""
            ]
            applicationJavascript
        )
      noiseUnit <- H.liftAff $ audioWorkletAddModule audioCtx objUrl (AudioWorkletNodeRequest :: WhiteNoiseRequest)
      H.liftEffect do
        unitCache <- makeUnitCache
        let
          ffiAudio = defaultFFIAudio audioCtx unitCache
        unsubscribe <-
          subscribe
            (run (pure unit) (pure { noiseUnit }) { easingAlgorithm } ffiAudio piece)
            (\(_ :: Run Unit ()) -> pure unit)
        pure { unsubscribe, audioCtx }
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect do
      unsubscribe
      for_ audioCtx close
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
