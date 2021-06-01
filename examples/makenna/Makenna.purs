module WAGS.Example.Makenna where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Foldable (foldl, for_)
import Data.Lens (over)
import Data.Lens.Lens.Tuple (_1)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Foreign.Object as O
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Heterogeneous.Mapping (hmap)
import Math (pow)
import WAGS.Change (change)
import WAGS.Control.Functions (loop, start, (@|>))
import WAGS.Control.Types (Frame0, Scene, Frame)
import WAGS.Create (create)
import WAGS.Graph.AudioUnit (TGain, TPeriodicOsc, TSpeaker)
import WAGS.Graph.Optionals (CPeriodicOsc, CSpeaker, CGain, gain, periodicOsc, speaker)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, makePeriodicWave, makeUnitCache)
import WAGS.Run (SceneI, run)

type Note
  = Number /\ Maybe Number

score :: Array Note
score =
  [ 2.0 /\ pure 70.0
  , 1.0 /\ pure 70.0
  , 3.0 /\ pure 72.0
  , 3.0 /\ pure 70.0
  , 3.0 /\ pure 75.0
  , 4.0 /\ pure 74.0
  , 2.0 /\ empty
  --
  , 2.0 /\ pure 70.0
  , 1.0 /\ pure 70.0
  , 3.0 /\ pure 72.0
  , 3.0 /\ pure 70.0
  , 3.0 /\ pure 77.0
  , 4.0 /\ pure 75.0
  , 2.0 /\ empty
  --
  , 2.0 /\ pure 70.0
  , 1.0 /\ pure 70.0
  , 3.0 /\ pure 82.0
  , 3.0 /\ pure 79.0
  , 3.0 /\ pure 75.0
  , 3.0 /\ pure 74.0
  , 4.0 /\ pure 72.0
  , 2.0 /\ empty
  --
  , 2.0 /\ pure 80.0
  , 1.0 /\ pure 80.0
  , 3.0 /\ pure 79.0
  , 3.0 /\ pure 75.0
  , 3.0 /\ pure 77.0
  , 6.0 /\ pure 75.0
  ]

type EnrichedNote
  = { start :: Number, dur :: Number, end :: Number } /\ Maybe Number

rest0 = { start: 0.0, dur: 2.0, end: 2.0 } /\ empty :: EnrichedNote

score' :: Array EnrichedNote
score' =
  let
    folded =
      foldl
        ( \{ cur, acc } (dur /\ pitch) ->
            let
              new =
                { start: (fst cur).end
                , dur
                , end: (fst cur).end + dur
                }
                  /\ ((_ - 19.0) <$> pitch)
            in
              { cur: new, acc: acc <> [ new ] }
        )
        { cur: rest0, acc: [ rest0 ] }
        score
  in
    folded.acc

inTempo = over _1 (hmap (_ * 0.3)) :: EnrichedNote -> EnrichedNote

score'' :: Array EnrichedNote
score'' = map inTempo score'

midiToCps :: Number -> Number
midiToCps i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

shortAsdr :: Number -> Number
shortAsdr t
  | t < 0.15 = calcSlope 0.0 0.0 0.15 0.15 t
  | t < 0.25 = calcSlope 0.15 0.15 0.25 0.05 t
  | t < 0.36 = calcSlope 0.25 0.05 0.36 0.0 t
  | otherwise = 0.0

asdr :: Number -> Number -> Number
asdr t d
  | d < 0.4 = shortAsdr t
  | t < 0.0 = 0.0
  | t < 0.2 = calcSlope 0.0 0.0 0.2 0.3 t
  | t < 0.4 = calcSlope 0.2 0.3 0.4 0.1 t
  | t < d - 0.06 = calcSlope 0.4 0.1 (d - 0.06) 0.0 t
  | otherwise = 0.0

type SceneTemplate
  = CSpeaker
      { gain :: CGain { osc :: CPeriodicOsc }
      }

type SceneType
  = { speaker :: TSpeaker /\ { gain :: Unit }
    , gain :: TGain /\ { osc :: Unit }
    , osc :: TPeriodicOsc /\ {}
    }

scene :: Number -> EnrichedNote -> SceneTemplate
scene time ({ start, dur } /\ pitch) =
  speaker
    { gain:
        gain
          (maybe 0.0 (const $ asdr (time - start) dur) pitch)
          { osc: periodicOsc "bday" (midiToCps (fromMaybe 60.0 pitch))
          }
    }

createFrame :: Frame (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0 SceneType (List EnrichedNote)
createFrame = do
  { time } <- ask
  pure (create (start $> scene time (inTempo rest0)) $> L.fromFoldable score'')

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
piece =
  createFrame
    @|> loop \fr -> do
        { time } <- ask
        let
          f i = case (extract i) of
            Nil -> i $> Nil
            (a : b)
              | time > (fst a).end -> f (i $> b)
              | otherwise -> change (i $> scene time a) $> (a : b)
        pure $ f fr

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

buttonStyle :: forall r i. HP.IProp ( style :: String | r ) i
buttonStyle = HP.style "padding: 3px; margin: 5px"

render :: forall m. State -> H.ComponentHTML Action () m
render =
  const do
    HH.div_
      [ HH.h1_
          [ HH.text "Happy Birthday, Makenna!" ]
      , HH.button
          [ HE.onClick \_ -> StartAudio, buttonStyle ]
          [ HH.text "Play" ]
      , HH.button
          [ HE.onClick \_ -> StopAudio, buttonStyle ]
          [ HH.text "Stop" ]
      ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    handleAction StopAudio
    { unsubscribe, audioCtx } <-
      H.liftEffect do
        audioCtx <- context
        unitCache <- makeUnitCache
        bday <-
          makePeriodicWave
            audioCtx
            (0.02 +> 0.3 +> -0.1 +> -0.25 +> V.empty)
            (-0.03 +> -0.25 +> 0.05 +> 0.2 +> V.empty)
        let
          ffiAudio =
            (defaultFFIAudio audioCtx unitCache)
              { periodicWaves = O.singleton "bday" bday
              }
        unsubscribe <-
          subscribe
            ( run (pure unit) (pure unit) { easingAlgorithm }
                (FFIAudio ffiAudio)
                piece
            )
            (const $ pure unit)
        pure { unsubscribe, audioCtx }
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect do
      unsubscribe
      for_ audioCtx close
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
