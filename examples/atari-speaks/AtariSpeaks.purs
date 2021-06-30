module WAGS.Example.AtariSpeaks where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.Foldable (for_)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
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
import Math (pi, sin)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CGain, CLoopBuf, CSpeaker, gain, loopBuf, speaker)
import WAGS.Graph.AudioUnit (TGain, TLoopBuf, TSpeaker)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Run (RunAudio, SceneI, RunEngine, run)

vol = 1.4 :: Number

type SceneTemplate
  = CSpeaker
      { gain0 :: CGain { loop0 :: CLoopBuf }
      , gain1 :: CGain { loop1 :: CLoopBuf }
      , gain2 :: CGain { loop2 :: CLoopBuf }
      }

type SceneType
  = { speaker :: TSpeaker /\ { gain0 :: Unit, gain1 :: Unit, gain2 :: Unit }
    , gain0 :: TGain /\ { loop0 :: Unit }
    , loop0 :: TLoopBuf /\ {}
    , gain1 :: TGain /\ { loop1 :: Unit }
    , loop1 :: TLoopBuf /\ {}
    , gain2 :: TGain /\ { loop2 :: Unit }
    , loop2 :: TLoopBuf /\ {}
    }

scene :: Number -> SceneTemplate
scene time =
  let
    rad = pi * time
  in
    speaker
      { gain0:
          gain (0.3 * vol)
            { loop0: loopBuf { playbackRate: 1.0 + 0.1 * sin rad } "atar"
            }
      , gain1:
          gain (0.15 * vol)
            { loop1:
                loopBuf
                  { playbackRate: 1.5 + 0.1 * sin (2.0 * rad)
                  , loopStart: 0.1 + 0.1 * sin rad
                  , loopEnd: 0.5 + 0.25 * sin (2.0 * rad)
                  }
                  "atar"
            }
      , gain2:
          gain (0.3 * vol)
            { loop2: loopBuf { playbackRate: 0.25 } "atar"
            }
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
        [ HH.text "Atari speaks" ]
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
    atar <-
      H.liftAff $ toAffE
        $ decodeAudioDataFromUri
            audioCtx
            "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache) { buffers = pure $ O.singleton "atar" atar }
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
