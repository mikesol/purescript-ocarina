module WAGS.Example.MediaElement where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable (for_)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.String.Utils (unsafeRepeat)
import Data.Tuple.Nested (type (/\))
import Data.UInt (toInt)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Log
import FRP.Event (subscribe)

vol = 1.4 :: Number

type World = { atar :: BrowserMediaElement }
type Analysers = (myAnalyser :: Maybe AnalyserNode)
type AnalysersCb = (myAnalyser :: AnalyserNodeCb)

type SceneTemplate
  = CSpeaker
  { analyse ::
      CAnalyser
        { analysed ::
            CGain
              { gain0 :: CGain { loop0 :: CMediaElement }
              }
        }
  }

type SceneType
  =
  { speaker :: TSpeaker /\ { analyser :: Unit }
  , analyser :: TAnalyser /\ { analysed :: Unit }
  , analysed :: TGain /\ { gain0 :: Unit }
  , gain0 :: TGain /\ { loop0 :: Unit }
  , loop0 :: TMediaElement /\ {}
  }

scene :: BehavingScene Unit World AnalysersCb -> SceneTemplate
scene (BehavingScene { time, world: { atar }, analyserCallbacks: { myAnalyser } }) =
  let
    rad = pi * time
  in
    speaker
      { analyse:
          analyser myAnalyser
            { analysed:
                gain 1.0
                  { gain0:
                      gain (0.5 + 0.4 * sin (rad * 3.0))
                        { loop0: mediaElement atar
                        }

                  }
            }
      }

piece :: Scene (BehavingScene Unit World AnalysersCb) RunAudio RunEngine Frame0 Unit
piece = (scene >>> icreate) @!> iloop \e _ -> ivoid $ ichange (scene e)

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
    atar <- H.liftEffect $ makeAudio_
    runUI (component atar) unit body

type State
  =
  { unsubscribe :: Effect Unit
  , unsubscribeFromHalogen :: Maybe H.SubscriptionId
  , audioCtx :: Maybe AudioContext
  , audio :: BrowserMediaElement
  , freqz :: Array String
  }

data Action
  = StartAudio
  | StopAudio
  | Freqz (Array String)

component :: forall query input output m. MonadEffect m => MonadAff m => BrowserMediaElement -> H.Component query input output m
component elt =
  H.mkComponent
    { initialState: const $ initialState elt
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: BrowserMediaElement -> State
initialState =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  , audio: _
  , unsubscribeFromHalogen: Nothing
  , freqz: []
  }

foreign import makeAudio_ :: Effect BrowserMediaElement
foreign import playAudio_ :: BrowserMediaElement -> Effect Unit
foreign import pauseAudio_ :: BrowserMediaElement -> Effect Unit

render :: forall m. State -> H.ComponentHTML Action () m
render { freqz } = do
  HH.div_
    $
      [ HH.h1_
          [ HH.text "Media element" ]
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
    { audio: atar } <- H.get
    { emitter, listener } <- H.liftEffect HS.create
    unsubscribeFromHalogen <- H.subscribe emitter
    audioCtx <- H.liftEffect context
    -- just for kicks
    H.liftEffect $ contextState audioCtx >>= Log.info
    H.liftAff $ toAffE $ contextResume audioCtx
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    H.liftEffect $ playAudio_ atar
    unsubscribe <-
      H.liftEffect
        $ subscribe
          (run (pure unit) (pure { atar }) { easingAlgorithm } (ffiAudio) piece)
          ( \({ analysers: { myAnalyser } } :: BehavingRun Unit Analysers) ->
              for_ myAnalyser \myAnalyser' -> do
                frequencyData <- getByteFrequencyData myAnalyser'
                arr <- toArray frequencyData
                HS.notify listener (Freqz ((map (\i -> unsafeRepeat (toInt i + 1) ">") arr)))
                pure unit
          )
    H.modify_ _ { unsubscribe = unsubscribe, unsubscribeFromHalogen = Just unsubscribeFromHalogen, audioCtx = Just audioCtx }
  Freqz freqz -> H.modify_ _ { freqz = freqz }
  StopAudio -> do
    { audio, unsubscribe, unsubscribeFromHalogen, audioCtx } <- H.get
    H.liftEffect unsubscribe
    H.liftEffect $ pauseAudio_ audio
    for_ unsubscribeFromHalogen H.unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
