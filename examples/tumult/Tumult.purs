module WAGS.Example.Tumult where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D1)
import Data.Vec (singleton, (+>))
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
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Types (Frame0, Scene, SubScene)
import WAGS.Create.Optionals (bandpass, gain, highpass, input, loopBuf, sinOsc, speaker, subgraph, tumult)
import WAGS.Graph.Paramable (paramize)
import WAGS.Graph.Parameter (AudioParameter, ff)
import WAGS.Interpret (class AudioInterpret, close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Run (BehavingRun, RunAudio, RunEngine, BehavingScene(..), run)
import WAGS.Tumult.Make (tumultuously)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World = { shruti :: BrowserAudioBuffer }

newtype SGWorld = SGWorld Number

globalFF = ff 0.06 :: AudioParameter -> AudioParameter

vec :: V.Vec D1 Unit
vec = V.fill (const unit)

subPiece1
  :: forall audio engine
   . AudioInterpret audio engine
  => Int
  -> SubScene "gnn" (beep :: Unit) SGWorld audio engine Frame0 Unit
subPiece1 _ = mempty # SG.loopUsingScene \(SGWorld time) _ ->
  { control: unit
  , scene:
      { gnn: tumult
          ( let
              sweep =
                { freq: globalFF $ paramize $ 3000.0 + sin (pi * time * 0.2) * 2990.0
                , q: globalFF $ paramize $ 1.0
                }
              tmod = time % 10.0
              tumult
                | tmod < 2.0 = tumultuously ({ output: input (Proxy :: _ "shruti") } +> V.empty)
                | tmod < 6.0 = tumultuously ({ output: highpass sweep (input (Proxy :: _ "shruti")) } +> V.empty)
                | otherwise = tumultuously
                    ( { output: gain 1.0
                          { hpf: bandpass sweep (input (Proxy :: _ "shruti"))
                          , bpf: bandpass { freq: 200.0 } (input (Proxy :: _ "shruti"))
                          , beep: gain (sin (time * pi * 10.0) * 0.2 + 0.1) { sosc: sinOsc 440.0 }
                          }
                      } +> V.empty
                    )
            in
              tumult
          )
          { shruti: input (Proxy :: _ "beep") }

      }
  }

piece :: Scene (BehavingScene Unit World ()) RunAudio RunEngine Frame0 Unit
piece = mempty # loopUsingScene \(BehavingScene env) _ ->
  { control: unit
  , scene: speaker
      { gn: gain 1.0
          { sg2: subgraph (singleton $ SGWorld env.time)
              (subPiece1)
              { beep: loopBuf env.world.shruti }
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
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    shruti <-
      H.liftAff $ decodeAudioDataFromUri
          audioCtx
          "https://freesound.org/data/previews/513/513742_153257-hq.mp3"
    unsubscribe <-
      H.liftEffect
        $ subscribe
          (run (pure unit) (pure { shruti }) { easingAlgorithm } (ffiAudio) piece)
          (\(_ :: BehavingRun Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
