module WAGS.Example.AtariSpeaks where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
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
import Type.Proxy (Proxy(..))
import WAGS (AudioContext, FFIAudio(..), Frame0, Gain, GetSetAP, LoopBuf, Scene, SceneI, Speaker, change, close, context, create, decodeAudioDataFromUri, defaultFFIAudio, env, gain, loop, loopBuf, makeUnitCache, run, speaker, start, (@>))
import WAGS.Control.Qualified as Ix

vol = 1.4 :: Number

scene ::
  Number ->
  Speaker
    ( (Gain GetSetAP (LoopBuf "atar" GetSetAP))
        /\ (Gain GetSetAP (LoopBuf "atar" GetSetAP))
        /\ (Gain GetSetAP (LoopBuf "atar" GetSetAP))
        /\ Unit
    )
scene time =
  let
    rad = pi * time
  in
    speaker
      $ ( ( gain (0.3 * vol)
              (loopBuf { playbackRate: 1.0 + 0.1 * sin rad } (Proxy :: _ "atar"))
          )
            /\ ( gain (0.15 * vol)
                  ( loopBuf
                      { playbackRate: 1.5 + 0.1 * sin (2.0 * rad)
                      , start: 0.1 + 0.1 * sin rad
                      , end: 0.5 + 0.25 * sin (2.0 * rad)
                      }
                      (Proxy :: _ "atar")
                  )
              )
            /\ ( gain (0.3 * vol)
                  (loopBuf { playbackRate: 0.25 } (Proxy :: _ "atar"))
              )
            /\ unit
        )

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
piece =
  Ix.do
    start
    { time } <- env
    create (scene time) $> Right unit
    @> loop
        ( const
            $ Ix.do
                { time } <- env
                ivoid $ change (scene time)
        )

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
      ffiAudio = (defaultFFIAudio audioCtx unitCache) { buffers = O.singleton "atar" atar }
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
