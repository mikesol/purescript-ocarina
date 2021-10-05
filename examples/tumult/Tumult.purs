module WAGS.Example.Tumult where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math (pi, sin, (%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions.Graph (loopUsingScene)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (bandpass, gain, highpass, input, loopBuf, lowpass, speaker, tumult)
import WAGS.Graph.Parameter (AudioParameter, ff)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, makeUnitCache)
import WAGS.Run (Run, RunAudio, RunEngine, SceneI(..), run)
import WAGS.Tumult.Make (tumultuously)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World = { shruti :: BrowserAudioBuffer }

newtype SGWorld = SGWorld Number

globalFF = ff 0.06 :: AudioParameter -> AudioParameter

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit
piece = unit # loopUsingScene \(SceneI env) _ ->
  { control: unit
  , scene: speaker
      { gn: gain 1.0
          { tumult: tumult
              ( let
                  sweep =
                    { freq: globalFF $ pure $ 2000.0 + sin (pi * env.time * 0.2) * 1990.0
                    , q: globalFF $ pure $ 1.0
                    }
                  tmod = env.time % 4.0
                  tumult
                    | tmod < 1.0 = tumultuously ({ output: input (Proxy :: _ "shruti") } +> V.empty)
                    | tmod < 2.0 = tumultuously ({ output: highpass sweep (input (Proxy :: _ "shruti")) } +> V.empty)
                    | tmod < 3.0 = tumultuously ({ output: lowpass sweep (input (Proxy :: _ "shruti")) } +> V.empty)
                    | otherwise = tumultuously ({ output: bandpass sweep (input (Proxy :: _ "shruti")) } +> V.empty)
                in
                  tumult
              )
              { shruti: loopBuf env.world.shruti }
          }
      }
  }

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
  , freqz :: Array String
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
  , freqz: []
  }

render :: forall m. State -> H.ComponentHTML Action () m
render { freqz } = do
  HH.div_
    $
      [ HH.h1_
          [ HH.text "Tumult test" ]
      , HH.button
          [ HE.onClick \_ -> StartAudio ]
          [ HH.text "Start audio" ]
      , HH.button
          [ HE.onClick \_ -> StopAudio ]
          [ HH.text "Stop audio" ]
      ]
        <> map (\freq -> HH.p [] [ HH.text freq ]) freqz

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    shruti <-
      H.liftAff $ toAffE
        $ decodeAudioDataFromUri
          audioCtx
          "https://freesound.org/data/previews/513/513742_153257-hq.mp3"
    let
      ffiAudio =
        { context: audioCtx
        , writeHead: 0.0
        , units: unitCache
        }
    unsubscribe <-
      H.liftEffect
        $ subscribe
          (run (pure unit) (pure { shruti }) { easingAlgorithm } (ffiAudio) piece)
          (\(_ :: Run Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
