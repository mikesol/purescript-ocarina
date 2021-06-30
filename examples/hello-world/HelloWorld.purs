module WAGS.Example.HelloWorld where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
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
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, makeUnitCache)
import WAGS.Run (RunAudio, SceneI, RunEngine, run)
import WAGS.Create.Optionals (CGain, CSpeaker, CSinOsc, gain, sinOsc, speaker)
import WAGS.Graph.AudioUnit (TGain, TSinOsc, TSpeaker)

type SceneTemplate
  = CSpeaker
      { gain0 :: CGain { sin0 :: CSinOsc }
      , gain1 :: CGain { sin1 :: CSinOsc }
      , gain2 :: CGain { sin2 :: CSinOsc }
      , gain3 :: CGain { sin3 :: CSinOsc }
      }

type SceneType
  = { speaker :: TSpeaker /\ { gain0 :: Unit, gain1 :: Unit, gain2 :: Unit, gain3 :: Unit }
    , gain0 :: TGain /\ { sin0 :: Unit }
    , sin0 :: TSinOsc /\ {}
    , gain1 :: TGain /\ { sin1 :: Unit }
    , sin1 :: TSinOsc /\ {}
    , gain2 :: TGain /\ { sin2 :: Unit }
    , sin2 :: TSinOsc /\ {}
    , gain3 :: TGain /\ { sin3 :: Unit }
    , sin3 :: TSinOsc /\ {}
    }

scene :: Number -> SceneTemplate
scene time =
  let
    rad = pi * time
  in
    speaker
      { gain0: gain 0.1 { sin0: sinOsc (440.0 + (10.0 * sin (2.3 * rad))) }
      , gain1: gain 0.25 { sin1: sinOsc (235.0 + (10.0 * sin (1.7 * rad))) }
      , gain2: gain 0.2 { sin2: sinOsc (337.0 + (10.0 * sin rad)) }
      , gain3: gain 0.1 { sin3: sinOsc (530.0 + (19.0 * (5.0 * sin rad))) }
      }

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece = (_.time >>> scene >>> icreate) @!> iloop \{ time } _ -> ivoid $ ichange (scene time)

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
        [ HH.text "Hello world" ]
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
    let
      ffiAudio = defaultFFIAudio audioCtx unitCache
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) (pure unit) { easingAlgorithm } (FFIAudio ffiAudio) piece)
            (const $ pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }