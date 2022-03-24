module WAGS.Example.MultiBuf where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Variant (inj)
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
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (speaker)
import WAGS.Change (ichange')
import WAGS.Graph.AudioUnit (MultiPlayBuf(..), TMultiPlayBuf, TSpeaker)
import WAGS.Graph.Parameter (MultiPlayBufOnOff(..))
import WAGS.Graph.Paramable (paramize)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Run (BehavingRun, RunAudio, RunEngine, BehavingScene(..), run)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World =
  { sample :: BrowserAudioBuffer
  }

type Graph =
  ( speaker :: TSpeaker /\ { multiPlayBuf :: Unit }
  , multiPlayBuf :: TMultiPlayBuf /\ {}
  )

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

initialize :: forall residuals. IxWAG RunAudio RunEngine Frame0 residuals () Graph Unit
initialize = icreate $ speaker { multiPlayBuf: MultiPlayBuf { playbackRate: paramize 1.0, onOff: MultiPlayBufOnOff (inj (Proxy :: _ "off") 0.0) } }

loop
  :: forall proof
   . BehavingScene Unit World ()
  -> Unit
  -> IxWAG RunAudio RunEngine proof Unit Graph Graph Unit
loop (BehavingScene _) _ = do
  ichange' (Proxy :: Proxy "multiPlayBuf") { onOff: MultiPlayBufOnOff (inj (Proxy :: _ "off") 0.0) }
  pure unit

scene :: Scene (BehavingScene Unit World ()) RunAudio RunEngine Frame0 Unit
scene = const initialize @!> iloop loop

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State =
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

render :: forall m. State -> H.ComponentHTML Action () m
render _ = do
  HH.div_
    $
      [ HH.h1_
          [ HH.text "MultiBuf" ]
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
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    sample <-
      H.liftAff $ decodeAudioDataFromUri
        audioCtx
        "https://freesound.org/data/previews/50/50843_489520-hq.mp3"
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) (pure { sample }) { easingAlgorithm } ffiAudio scene)
            (\(_ :: BehavingRun Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
