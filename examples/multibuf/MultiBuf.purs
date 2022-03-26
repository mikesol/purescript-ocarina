module WAGS.Example.MultiBuf where

import Prelude

import Control.Parallel.Class (parallel, sequential)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Variant (inj)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe, create)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (speaker)
import WAGS.Graph.AudioUnit (MultiPlayBuf(..), TMultiPlayBuf, TSpeaker)
import WAGS.Graph.Paramable (paramize)
import WAGS.Graph.Parameter (MultiPlayBufOnOff(..))
import WAGS.Interpret (close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Run (TriggeredRun, RunAudio, RunEngine, TriggeredScene(..), runNoLoop)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World =
  { sample0 :: BrowserAudioBuffer
  , sample1 :: BrowserAudioBuffer
  , sample2 :: BrowserAudioBuffer
  , sample3 :: BrowserAudioBuffer
  }

type Graph =
  ( speaker :: TSpeaker /\ { multiPlayBuf :: Unit }
  , multiPlayBuf :: TMultiPlayBuf /\ {}
  )

alternate
  :: BrowserAudioBuffer -> BrowserAudioBuffer -> Number -> MultiPlayBufOnOff
alternate sample0 sample1 startTime = MultiPlayBufOnOff $ inj (Proxy :: _ "ons")
  { starts: { b: sample0, t: startTime, o: 0.0 }
  , next:
      [ { b: sample1, t: 1.0, o: 0.0 }
      , { b: sample0, t: 1.0, o: 0.0 }
      , { b: sample1, t: 1.0, o: 0.0 }
      , { b: sample0, t: 1.0, o: 0.0 }
      , { b: sample1, t: 1.0, o: 0.0 }
      , { b: sample0, t: 1.0, o: 0.0 }
      ]
  }

initialize
  :: forall residuals
   . TriggeredScene Unit World ()
  -> IxWAG RunAudio RunEngine Frame0 residuals () Graph Unit
initialize (TriggeredScene { world: { sample0, sample1 } }) = icreate $ speaker
  { multiPlayBuf:
      MultiPlayBuf
        { playbackRate: paramize 1.0
        , onOff: alternate sample0 sample1 0.0
        }
  }

loop
  :: forall proof
   . TriggeredScene Unit World ()
  -> Unit
  -> IxWAG RunAudio RunEngine proof Unit Graph Graph Unit
loop (TriggeredScene { world: { sample2, sample3 } }) _ = do
  ichange' (Proxy :: Proxy "multiPlayBuf")
    { onOff: alternate sample2 sample3 0.5
    }
  ichange' (Proxy :: Proxy "multiPlayBuf")
    { playbackRate: 1.5
    }
  ichange' (Proxy :: Proxy "multiPlayBuf")
    { onOff: MultiPlayBufOnOff (inj (Proxy :: _ "off") 4.0)
    }

scene :: Scene (TriggeredScene Unit World ()) RunAudio RunEngine Frame0 Unit
scene = initialize @!> iloop loop

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

component
  :: forall query input output m
   . MonadEffect m
  => MonadAff m
  => H.Component query input output m
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

handleAction
  :: forall output m
   . MonadEffect m
  => MonadAff m
  => Action
  -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    myEvent <- H.liftEffect create
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    let pf = parallel <<< decodeAudioDataFromUri audioCtx
    { sample0, sample1, sample2, sample3 } <- H.liftAff
      ( sequential
          ( { sample0: _, sample1: _, sample2: _, sample3: _ }
              <$> pf
                "https://freesound.org/data/previews/320/320873_527080-lq.mp3"
              <*> pf
                "https://freesound.org/data/previews/144/144971_2137927-lq.mp3"
              <*> pf
                "https://freesound.org/data/previews/110/110212_1751865-lq.mp3"
              <*>
                pf
                  "https://freesound.org/data/previews/110/110158_649468-lq.mp3"
          )
      )
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop myEvent.event
                (pure { sample0, sample1, sample2, sample3 })
                {}
                ffiAudio
                scene
            )
            (\(_ :: TriggeredRun Unit ()) -> pure unit)
    H.liftEffect $ myEvent.push unit
    H.liftAff $ do
      delay (Milliseconds 2500.0)
      H.liftEffect (myEvent.push unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
