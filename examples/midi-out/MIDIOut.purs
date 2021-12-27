module WAGS.Example.MIDIOut where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Monad.Indexed.Qualified as Ix
import Control.Plus (empty)
import Control.Promise (toAffE)
import Data.Foldable (foldl, for_)
import Data.Int (floor)
import Data.Lens (over)
import Data.Lens.Lens.Tuple (_1)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import FRP.Event.MIDI (MIDIEvent(..), midiAccess, sendMIDIEvent, toOutputMap)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Heterogeneous.Mapping (hmap)
import WAGS.Control.Functions (imodifyRes)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CConstant, CSpeaker, constant, speaker)
import WAGS.Graph.AudioUnit (TConstant, TSpeaker)
import WAGS.Interpret (close, context, makeFFIAudioSnapshot, makePeriodicWave)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.WebAPI (AudioContext, BrowserPeriodicWave)

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

type SceneTemplate
  = CSpeaker
  { cst :: CConstant
  }

type SceneType
  =
  ( speaker :: TSpeaker /\ { cst :: Unit }
  , cst :: TConstant /\ {}
  )

scene :: SceneTemplate
scene = speaker { cst: constant 0.0 }

type World = { bday :: BrowserPeriodicWave }

type Res = List EnrichedNote

createFrame
  :: SceneI Unit World ()
  -> IxWAG RunAudio RunEngine Frame0 Res () SceneType (List EnrichedNote)
createFrame _ = icreate scene $> L.fromFoldable score''

headroom = 0.02 :: Number

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Res
piece =
  createFrame
    @!> iloop \(SceneI { time }) l ->
      let
        f = case _ of
          Nil -> ipure Nil
          (a : b)
            | time > (fst a).start + headroom -> Ix.do
                _ <- imodifyRes (Cons a)
                f b
            | otherwise -> ipure (a : b)
      in
        f l

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

buttonStyle :: forall r i. HP.IProp (style :: String | r) i
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

handleAction
  :: forall output m
   . MonadEffect m
  => MonadAff m
  => Action
  -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    handleAction StopAudio
    { unsubscribe, audioCtx } <- do
      macc <- H.liftAff $ toAffE midiAccess
      H.liftEffect do
        omap <- toOutputMap macc
        audioCtx <- context
        ffiAudio <- makeFFIAudioSnapshot audioCtx
        bday <-
          makePeriodicWave
            audioCtx
            (0.02 +> 0.3 +> -0.1 +> -0.25 +> V.empty)
            (-0.03 +> -0.25 +> 0.05 +> 0.2 +> V.empty)
        unsubscribe <-
          subscribe
            (run (pure unit) (pure { bday }) { easingAlgorithm } ffiAudio piece)
            ( \({ time, res } :: Run Res ()) -> for_ res \r -> do
                for_ omap \opt ->
                  maybe (pure unit)
                    ( \pt -> sendMIDIEvent opt
                        (NoteOn 0 (floor pt) 100)
                        (wrap (max 0.0 $ (fst r).start - time))
                    )
                    (snd r)
            )
        pure { unsubscribe, audioCtx }
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect do
      unsubscribe
      for_ audioCtx close
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
