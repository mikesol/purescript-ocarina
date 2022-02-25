module WAGS.Example.WTK where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.Array (fromFoldable, singleton)
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import FRP.Event.MIDI (midi, midiAccess, midiInputDevices, MIDIDevice)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import WAGS.Example.WTK.RenderingEnv (makeRenderingEnv)
import WAGS.Example.WTK.TLP (piece)
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Run (BehavingRun, bufferToList, run)
import WAGS.WebAPI (AudioContext)

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm = let fOf initialTime = mkCofree initialTime \adj -> fOf $ max 10 (initialTime - adj) in fOf 20

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State
  =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  , devices :: List MIDIDevice
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
  , devices: Nil
  }

render :: forall m. State -> H.ComponentHTML Action () m
render s = HH.div_ (ui <> dev)
  where
  ui =
    [ HH.h1_
        [ HH.text "The Well-Typed Klavier" ]
    , HH.button
        [ HE.onClick \_ -> StartAudio ]
        [ HH.text "Start audio" ]
    , HH.button
        [ HE.onClick \_ -> StopAudio ]
        [ HH.text "Stop audio" ]
    ]
  dev = case s.devices of
    Nil -> []
    devices ->
      [ HH.h4_
          [ HH.text "Available input devices"]
      , HH.ul_ (fromFoldable $ map (HH.li_ <<< singleton <<< HH.text <<< showDevices) devices)
      ]
  showDevices d =
    let manufacturer = if d.manufacturer == "" then "" else d.manufacturer <> ": "
     in manufacturer <> d.name

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    midAcc <- H.liftAff $ toAffE midiAccess
    midDev <- H.liftEffect $ midiInputDevices midAcc
    -- alt Nil for thunk
    let
      trigger = (bufferToList 5 (midi midAcc)) <|> pure Nil
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    unsubscribe <-
      H.liftEffect
        $ subscribe
          ( run
              trigger
              (pure unit)
              { easingAlgorithm }
              (ffiAudio)
              (piece { makeRenderingEnv })
          )
          (\(_ :: BehavingRun Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx, devices = midDev }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing, devices = Nil }
