module WAGS.Example.NoLoop where

import Prelude

import Control.Alt ((<|>))
import Data.Array.NonEmpty (NonEmptyArray, fromArray, fromNonEmpty)
import Data.Foldable (for_)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (fold, subscribe)
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
import WAGS.Create.Optionals (CGain, CSpeaker, CSinOsc, gain, sinOsc, speaker)
import WAGS.Graph.AudioUnit (TGain, TSinOsc, TSpeaker)
import WAGS.Graph.Parameter (AudioEnvelope(..))
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Run (TriggeredScene(..), RunAudio, RunEngine, TriggeredRun, runNoLoop)
import WAGS.WebAPI (AudioContext)

defaultEnv :: NonEmptyArray Number
defaultEnv = fromNonEmpty (0.0 :| [ 0.0 ])
env
  :: { v :: Array Number
     , o :: Number
     , d :: Number
     }
  -> AudioEnvelope

env { v, o, d } = AudioEnvelope
  { values: fromMaybe defaultEnv $ fromArray v
  , timeOffset: o
  , duration: d
  }

type SceneTemplate
  = CSpeaker
  { gain0 :: CGain { sin0 :: CSinOsc }
  , gain1 :: CGain { sin1 :: CSinOsc }
  , gain2 :: CGain { sin2 :: CSinOsc }
  , gain3 :: CGain { sin3 :: CSinOsc }
  }

type SceneType
  =
  { speaker :: TSpeaker /\ { gain0 :: Unit, gain1 :: Unit, gain2 :: Unit, gain3 :: Unit }
  , gain0 :: TGain /\ { sin0 :: Unit }
  , sin0 :: TSinOsc /\ {}
  , gain1 :: TGain /\ { sin1 :: Unit }
  , sin1 :: TSinOsc /\ {}
  , gain2 :: TGain /\ { sin2 :: Unit }
  , sin2 :: TSinOsc /\ {}
  , gain3 :: TGain /\ { sin3 :: Unit }
  , sin3 :: TSinOsc /\ {}
  }

scene :: Number -> SceneTemplate
scene mult =
  speaker
    { gain0: gain
        ( env
            { v: [ 0.0, 0.2, 0.05, 0.025, 0.0 ]
            , o: 0.0
            , d: 2.0
            }
        )
        { sin0: sinOsc (220.0 * mult) }
    , gain1: gain
        ( env
            { v: [ 0.0, 0.05, 0.025, 0.025, 0.0 ]
            , o: 0.0
            , d: 2.0
            }
        )
        { sin1: sinOsc (220.0 * mult * 2.0) }
    , gain2: gain
        ( env
            { v: [ 0.0, 0.3, 0.05, 0.025, 0.0 ]
            , o: 0.0
            , d: 2.0
            }
        )
        { sin2: sinOsc (220.0 * mult * 3.0) }
    , gain3: gain
        ( env
            { v: [ 0.0, 0.05, 0.025, 0.025, 0.0 ]
            , o: 0.0
            , d: 2.0
            }
        )
        { sin3: sinOsc (220.0 * mult * 4.0) }
    }

piece :: Scene (TriggeredScene Number Unit ()) RunAudio RunEngine Frame0 Unit
piece = (unwrap >>> _.trigger >>> scene >>> icreate) @!> iloop \(TriggeredScene { trigger }) _ -> ivoid $ ichange (scene trigger)

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
        [ HH.text "No loop rendering" ]
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
    unsubscribe <-
      H.liftEffect
        $ subscribe
          ( runNoLoop
              (( fold
                  ( \_ (b /\ u) ->
                      if b >= 4.0 then (b - 1.0) /\ false else if b <= 1.0 then (b + 1.0) /\ true else (if u then add else sub) b 1.0 /\ u
                  )
                  (interval 2000)
                  (1.0 /\ true) <#> fst
              ) <|> pure 1.0 )
              (pure unit)
              {}
              ffiAudio
              piece
          )
          (\(_ :: TriggeredRun Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }