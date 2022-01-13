module WAGS.Example.SMC2022 where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\))
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
import Math (pi, sin)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop, loopUsingScene, (@!>))
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Types (Frame0, Scene, SubScene(..))
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CGain, CSinOsc, CSpeaker, gain, sinOsc, speaker, tumult)
import WAGS.Graph.AudioUnit (TGain, TSinOsc, TSpeaker)
import WAGS.Interpret (class AudioInterpret, close, context, makeFFIAudioSnapshot)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.Tumult.Make (tumultuously)
import WAGS.WebAPI (AudioContext)

fsin0 time = sin (time * pi) * 30.0 + 100.0

-- for x in range(3,30): print(', gain'+str(x)+': gain 0.01 { sin'+str(x)+': sinOsc '+str(float(x*100))+' }')

scene' time =
  { gain0: gain 0.01 { sin0: sinOsc $ fsin0 time }
  , gain1: gain 0.01 { sin1: sinOsc 200.0 }
  , gain2: gain 0.01 { sin2: sinOsc 200.0 }
  , gain3: gain 0.01 { sin3: sinOsc 300.0 }
  , gain4: gain 0.01 { sin4: sinOsc 400.0 }
  , gain5: gain 0.01 { sin5: sinOsc 500.0 }
  , gain6: gain 0.01 { sin6: sinOsc 600.0 }
  , gain7: gain 0.01 { sin7: sinOsc 700.0 }
  , gain8: gain 0.01 { sin8: sinOsc 800.0 }
  , gain9: gain 0.01 { sin9: sinOsc 900.0 }
  , gain10: gain 0.01 { sin10: sinOsc 1000.0 }
  , gain11: gain 0.01 { sin11: sinOsc 1100.0 }
  , gain12: gain 0.01 { sin12: sinOsc 1200.0 }
  , gain13: gain 0.01 { sin13: sinOsc 1300.0 }
  , gain14: gain 0.01 { sin14: sinOsc 1400.0 }
  , gain15: gain 0.01 { sin15: sinOsc 1500.0 }
  , gain16: gain 0.01 { sin16: sinOsc 1600.0 }
  , gain17: gain 0.01 { sin17: sinOsc 1700.0 }
  , gain18: gain 0.01 { sin18: sinOsc 1800.0 }
  , gain19: gain 0.01 { sin19: sinOsc 1900.0 }
  , gain20: gain 0.01 { sin20: sinOsc 2000.0 }
  , gain21: gain 0.01 { sin21: sinOsc 2100.0 }
  , gain22: gain 0.01 { sin22: sinOsc 2200.0 }
  , gain23: gain 0.01 { sin23: sinOsc 2300.0 }
  , gain24: gain 0.01 { sin24: sinOsc 2400.0 }
  , gain25: gain 0.01 { sin25: sinOsc 2500.0 }
  , gain26: gain 0.01 { sin26: sinOsc 2600.0 }
  , gain27: gain 0.01 { sin27: sinOsc 2700.0 }
  , gain28: gain 0.01 { sin28: sinOsc 2800.0 }
  , gain29: gain 0.01 { sin29: sinOsc 2900.0 }
  }

scene time = speaker (scene' time)

pieceF :: Scene (SceneI Unit Unit ()) RunAudio RunEngine Frame0 Unit
pieceF = (unwrap >>> _.time >>> scene >>> icreate) @!> iloop \(SceneI { time }) _ -> ivoid $ ichange { sin0: fsin0 time }

pieceS :: Scene (SceneI Unit Unit ()) RunAudio RunEngine Frame0 Unit
pieceS = mempty # loopUsingScene \(SceneI env) _ ->
  { control: unit
  , scene: speaker
      { gn: gain 1.0
          { tmt: tumult
              ( tumultuously ({ output: gain 1.0 (scene' env.time) } +> V.empty)
              )
              {}
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
  , whichExample :: WhichExample
  }

data WhichExample = Fast | Slow

data Action
  = StartAudio
  | StopAudio
  | SetExample WhichExample

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
  , whichExample: Fast
  }

render :: forall m. State -> H.ComponentHTML Action () m
render { whichExample } = do
  HH.div_
    [ HH.h1_
        [ HH.text "Hello world" ]
    , HH.button
        [ HE.onClick \_ -> StartAudio ]
        [ HH.text "Start audio" ]
    , HH.button
        [ HE.onClick \_ -> StopAudio ]
        [ HH.text "Stop audio" ]
    , HH.button
        [ HE.onClick \_ -> SetExample case whichExample of
            Fast -> Slow
            Slow -> Fast
        ]
        [ HH.text case whichExample of
            Fast -> "use runtime rep"
            Slow -> "use compile-time rep"
        ]
    ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    { whichExample } <- H.get
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    unsubscribe <-
      H.liftEffect
        $ subscribe
          ( run (pure unit) (pure unit) { easingAlgorithm } ffiAudio case whichExample of
              Fast -> pieceF
              Slow -> pieceS
          )
          (\(_ :: Run Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  SetExample whichExample -> H.modify_ _ { whichExample = whichExample }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }