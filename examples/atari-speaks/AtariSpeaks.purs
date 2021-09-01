module WAGS.Example.AtariSpeaks where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable (for_, intercalate)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (null)
import Data.String.Utils (unsafeRepeat)
import Data.Tuple.Nested (type (/\))
import Data.UInt (toInt)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CGain, CLoopBuf, CSpeaker, CAnalyser, analyser, gain, loopBuf, speaker)
import WAGS.Graph.AudioUnit (TAnalyser, TGain, TLoopBuf, TSpeaker)
import WAGS.Interpret (AnalyserNode, AudioContext, BrowserAudioBuffer, close, context, decodeAudioDataFromUri, getByteFrequencyData, makeUnitCache)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)

type Assets
  = ( buffers :: { atar :: BrowserAudioBuffer }
    , periodicWaves :: {}
    , floatArrays :: {}
    , recorders :: {}
    , worklets :: {}
    , analysers :: { myAnalyser :: Maybe AnalyserNode }
    )

vol = 1.4 :: Number

type SceneTemplate
  = CSpeaker
      { analyse ::
          CAnalyser "myAnalyser"
            { analysed ::
                CGain
                  { gain0 :: CGain { loop0 :: CLoopBuf "atar" }
                  , gain1 :: CGain { loop1 :: CLoopBuf "atar" }
                  , gain2 :: CGain { loop2 :: CLoopBuf "atar" }
                  }
            }
      }

type SceneType
  = { speaker :: TSpeaker /\ { analyser :: Unit }
    , analyser :: TAnalyser "myAnalyser" /\ { analysed :: Unit }
    , analysed :: TGain /\ { gain0 :: Unit, gain1 :: Unit, gain2 :: Unit }
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
      { analyse:
          analyser (Proxy :: _ "myAnalyser")
            { analysed:
                gain 1.0
                  { gain0:
                      gain (0.3 * vol)
                        { loop0: loopBuf { playbackRate: 1.0 + 0.1 * sin rad } (Proxy :: _ "atar")
                        }
                  , gain1:
                      gain (0.15 * vol)
                        { loop1:
                            loopBuf
                              { playbackRate: 1.5 + 0.1 * sin (2.0 * rad)
                              , loopStart: 0.1 + 0.1 * sin rad
                              , loopEnd: 0.5 + 0.25 * sin (2.0 * rad)
                              }
                              (Proxy :: _ "atar")
                        }
                  , gain2:
                      gain (0.3 * vol)
                        { loop2: loopBuf { playbackRate: 0.25 } (Proxy :: _ "atar")
                        }
                  }
            }
      }

piece :: Scene (SceneI Unit Unit) Assets RunAudio RunEngine Frame0 Unit
piece = (unwrap >>> _.time >>> scene >>> icreate) @!> iloop \(SceneI { time }) _ -> ivoid $ ichange (scene time)

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
    , freqz :: Array String
    }

data Action
  = StartAudio
  | StopAudio
  | Freqz (Array String)

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
    $ [ HH.h1_
          [ HH.text "Atari speaks" ]
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
    { emitter, listener } <- H.liftEffect HS.create
    unsubscribeFromHalogen <- H.subscribe emitter
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    atar <-
      H.liftAff $ toAffE
        $ decodeAudioDataFromUri
            audioCtx
            "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"
    let
      ffiAudio =
        { context: audioCtx
        , writeHead: 0.0
        , units: unitCache
        , worklets: {}
        , microphone: pure null
        , recorders: pure {}
        , buffers: pure { atar }
        , floatArrays: pure {}
        , periodicWaves: pure {}
        }
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) (pure unit) { easingAlgorithm } (ffiAudio) piece)
            ( \{ analysers: { myAnalyser } } ->
                for_ myAnalyser \myAnalyser' -> do
                  frequencyData <- getByteFrequencyData myAnalyser'
                  arr <- toArray frequencyData
                  HS.notify listener (Freqz ((map (\i -> unsafeRepeat (toInt i + 1) ">") arr)))
                  pure unit
            )
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  Freqz freqz -> H.modify_ _ { freqz = freqz }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
