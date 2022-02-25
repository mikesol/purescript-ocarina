module WAGS.Example.Subgraph where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D40)
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
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions.Graph (loopUsingScene)
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Types (Frame0, Scene, SubScene)
import WAGS.Create.Optionals (gain, input, playBuf, sinOsc, speaker, subgraph)
import WAGS.Graph.AudioUnit (_off, _on)
import WAGS.Interpret (class AudioInterpret, close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Run (Run, RunAudio, RunEngine, BehavingScene(..), run)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World = { atar :: BrowserAudioBuffer }

newtype SGWorld = SGWorld Number

vec :: V.Vec D40 Unit
vec = V.fill (const unit)

subPiece0
  :: forall audio engine
   . AudioInterpret audio engine
  => Int
  -> BrowserAudioBuffer
  -> SubScene "buffy" () SGWorld audio engine Frame0 Unit
subPiece0 i atar = mempty # SG.loopUsingScene \(SGWorld time) _ ->
  { control: unit
  , scene: { buffy: playBuf { onOff: if time < toNumber (i * 2) + 1.0 then _off else _on } atar }
  }

subPiece1
  :: forall audio engine
   . AudioInterpret audio engine
  => Int
  -> SubScene "gnn" (beep :: Unit) SGWorld audio engine Frame0 Unit
subPiece1 i = mempty # SG.loopUsingScene \(SGWorld time) _ ->
  { control: unit
  , scene:
      { gnn: gain
          (if time >= toNumber (i * 2) + 1.0 && time < toNumber (i * 2) + 1.2 then 0.10 else 0.0)
          (input (Proxy :: _ "beep"))
      }
  }

piece :: Scene (BehavingScene Unit World ()) RunAudio RunEngine Frame0 Unit
piece = mempty # loopUsingScene \(BehavingScene env) _ ->
  { control: unit
  , scene: speaker
      { gn: gain 1.0
          { sg: subgraph vec
              (\i _ -> subPiece0 i env.world.atar)
              (const $ const $ SGWorld env.time)
              {}
          , sg2: subgraph vec
              (\i _ -> subPiece1 i)
              (const $ const $ SGWorld env.time)
              { beep: sinOsc 440.0 }
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
          [ HH.text "Subgraph test" ]
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
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    atar <-
      H.liftAff $ decodeAudioDataFromUri
        audioCtx
        "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"
    unsubscribe <-
      H.liftEffect
        $ subscribe
          (run (pure unit) (pure { atar }) { easingAlgorithm } (ffiAudio) piece)
          (\(_ :: Run Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
