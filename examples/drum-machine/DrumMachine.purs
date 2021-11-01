module WAGS.Example.DrumMachine where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree, head, mkCofree, tail)
import Data.Foldable (for_)
import Data.Identity (Identity(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref
import FRP.Behavior (behavior)
import FRP.Event (makeEvent, subscribe)
import FRP.Event.Time (interval)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CGain, CSpeaker, CPlayBuf, gain, speaker, playBuf)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TLoopBuf, TSpeaker)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, makeUnitCache)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type SceneTemplate
  = CSpeaker
  { gain0 :: CGain { play0 :: CPlayBuf }
  }

type SceneType
  =
  { speaker :: TSpeaker /\ { gain0 :: Unit }
  , gain0 :: TGain /\ { play0 :: Unit }
  , play0 :: TLoopBuf /\ {}
  }

gap = 0.27 :: Number

type World = { snare :: BrowserAudioBuffer }

scene :: Boolean -> SceneI Unit World () -> SceneTemplate
scene shouldReset (SceneI { time, world: { snare } }) =
  let
    tgFloor = floor (time / gap)
  in
    speaker
      { gain0:
          gain 1.0
            { play0:
                playBuf
                  { onOff:
                      if (not shouldReset) then
                        pure On
                      else
                        ff ((toNumber (tgFloor + 1) * gap) - time) (pure OffOn)
                  }
                  snare
            }
      }

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit
piece =
  (\e -> icreate (scene false e) $> 0.0)
    @!> iloop \(SceneI e) lastCrossing ->
      let
        tgFloor = floor (e.time / gap)

        crossingDivide = tgFloor /= floor ((e.time + 0.06) / gap)

        crossDiff = e.time - lastCrossing

        shouldReset = crossingDivide && crossDiff > 0.2
      in
        ichange (scene shouldReset (SceneI e)) $> (if shouldReset then e.time else lastCrossing)

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
    [ HH.h1_
        [ HH.text "Drum Machine" ]
    , HH.button
        [ HE.onClick \_ -> StartAudio ]
        [ HH.text "Start audio" ]
    , HH.button
        [ HE.onClick \_ -> StopAudio ]
        [ HH.text "Stop audio" ]
    ]

drumCf :: Cofree Identity String
drumCf =
  deferCofree \_ ->
    "https://freesound.org/data/previews/321/321132_1337335-hq.mp3"
      /\ Identity
        ( deferCofree \_ ->
            "https://freesound.org/data/previews/331/331589_5820980-hq.mp3"
              /\ Identity
                ( deferCofree \_ ->
                    "https://freesound.org/data/previews/84/84478_377011-hq.mp3"
                      /\ Identity
                        ( deferCofree \_ ->
                            "https://freesound.org/data/previews/270/270156_1125482-hq.mp3"
                              /\ Identity
                                ( deferCofree \_ ->
                                    "https://freesound.org/data/previews/207/207956_19852-hq.mp3"
                                      /\ Identity drumCf
                                )
                        )
                )
        )

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    ibuf <-
      H.liftAff $ decodeAudioDataFromUri
        audioCtx
        (head drumCf)
    rf <- H.liftEffect (Ref.new (unwrap (tail drumCf)))
    bf <- H.liftEffect (Ref.new ibuf)
    ivlsub <-
      H.liftEffect
        $ subscribe (interval 1000) \_ -> do
          cf <- Ref.read rf
          Ref.write (unwrap (tail cf)) rf
          launchAff_ do
            buf <- decodeAudioDataFromUri audioCtx (head cf)
            H.liftEffect $ Ref.write buf bf
    let
      ffiAudio =
        { context: audioCtx
        , writeHead: 0.0
        , units: unitCache
        }
    unsubscribe <-
      H.liftEffect
        $ subscribe
          ( run (pure unit)
              ( { snare: _ }
                  <$> behavior \eAToB ->
                    makeEvent \fB ->
                      subscribe eAToB \aToB -> Ref.read bf >>= fB <<< aToB
              )
              { easingAlgorithm }
              (ffiAudio)
              piece
          )
          (\(_ :: Run Unit ()) -> pure unit)
    H.modify_
      _
        { unsubscribe =
            do
              unsubscribe
              ivlsub
        , audioCtx = Just audioCtx
        }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
