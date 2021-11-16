module WAGS.Example.SkipMachine where

import Prelude

import Control.Applicative.Indexed (ipure, (:*>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Plus (empty)
import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty(..))
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
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (playBuf, speaker)
import WAGS.Graph.AudioUnit (OnOff(..), TPlayBuf, TSpeaker)
import WAGS.Interpret (class AudioInterpret, bufferDuration, close, context, decodeAudioDataFromUri, makeUnitCache)
import WAGS.Patch (ipatch)
import WAGS.Run (Run, RunAudio, RunEngine, SceneI(..), run)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

vol = 1.4 :: Number

type World = { hamlet :: BrowserAudioBuffer }

type SceneType
  =
  ( speaker :: TSpeaker /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

type Cf = Cofree ((->) Number) (Maybe Int)
type FCf = Number -> Cf

myChange
  :: forall trigger analyserCb audio engine proof res
   . AudioInterpret audio engine
  => SceneI trigger World analyserCb
  -> FCf
  -> IxWAG audio engine proof res SceneType SceneType FCf
myChange (SceneI { time, world: { hamlet } }) fcf =
  maybe (ipure unit)
    ( \v -> ichange
        ( speaker
            { buf:
                playBuf { onOff: OffOn, bufferOffset: bufferDuration hamlet * (toNumber v) / (toNumber mxordr) } hamlet
            }
        )
    )
    hd $> unwrapCofree actualized
  where
  actualized = fcf time
  hd = extract actualized

order :: NonEmpty Array Int
order = NonEmpty 0 [ 9, 11, 4, 1, 12, 7, 6, 15, 8, 13, 10, 2, 5, 3, 14 ]

mxordr = foldl max 0 order :: Int

hnea :: forall a. NonEmpty Array a -> a
hnea (NonEmpty a _) = a

rotate :: forall a. NonEmpty Array a -> NonEmpty Array a
rotate (NonEmpty a b) = NonEmpty (fromMaybe a $ Array.head arr) (fromMaybe [] $ Array.tail arr)
  where
  arr = b <> [ a ]

cf :: NonEmpty Array Int -> Number -> FCf
cf nea len = f nea 0.0
  where
  maxnea = (foldl max 0 nea) + 1
  section = len / toNumber maxnea
  f ct x n = let hit = n + 0.04 > x in mkCofree (if hit then Just (hnea ct) else Nothing) (f (if hit then (rotate ct) else ct) (if hit then x + section else x))

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit
piece = (\e@(SceneI { world: { hamlet } }) -> ipatch { microphone: empty } :*> myChange e (cf order (bufferDuration hamlet))) @!> iloop myChange

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
    $
      [ HH.h1_
          [ HH.text "Skip machine" ]
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
    unitCache <- H.liftEffect makeUnitCache
    hamlet <-
      H.liftAff $ decodeAudioDataFromUri
          audioCtx
          "https://freesound.org/data/previews/50/50843_489520-hq.mp3"
    let
      ffiAudio =
        { context: audioCtx
        , writeHead: 0.0
        , units: unitCache
        }
    unsubscribe <-
      H.liftEffect
        $ subscribe
          (run (pure unit) (pure { hamlet }) { easingAlgorithm } (ffiAudio) piece)
          (\(_ :: Run Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  Freqz freqz -> H.modify_ _ { freqz = freqz }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
