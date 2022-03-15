module WAGS.Example.Patching where

import Prelude

import Control.Alt ((<|>))
import Control.Apply.Indexed ((:*>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Foldable (for_)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (class Lt, class Pred, D0, D40, d39, pred)
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import FRP.Event.Mouse (down, up)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene, SubScene)
import WAGS.Create.Optionals (gain, input, playBuf, subgraphSingleSetter)
import WAGS.Graph.AudioUnit (TGain, TSinOsc, TSpeaker, TSubgraph)
import WAGS.Graph.Parameter (_off, _on)
import WAGS.Interpret (class AudioInterpret, AsSubgraph(..), close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Patch (PatchedSubgraphInput(..), ipatch)
import WAGS.Run (RunAudio, RunEngine, TriggeredRun, TriggeredScene(..), runNoLoop)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World = { atar :: BrowserAudioBuffer }

newtype SGWorld = SGWorld Boolean

vec :: V.Vec D40 Unit
vec = V.fill (const unit)

newtype TriggerSG = TriggerSG
  ( forall proof
     . IxWAG RunAudio RunEngine proof Unit FullGraph FullGraph Unit
  )

unTriggerSG
  :: TriggerSG
  -> forall proof
   . IxWAG RunAudio RunEngine proof Unit FullGraph FullGraph Unit
unTriggerSG (TriggerSG ta) = ta

class CofreeSubgraph0 (n :: Type) where
  cofreeSubgraph0 :: n -> Cofree Identity TriggerSG

instance CofreeSubgraph0 D0 where
  cofreeSubgraph0 n = deferCofree
    ( \_ -> Tuple
        ( TriggerSG
            ( ichange' (Proxy :: _ "sg") (subgraphSingleSetter n (SGWorld true)) *> ichange' (Proxy :: _ "sg2") (subgraphSingleSetter n (SGWorld true))
            )
        )
        ( Identity $ deferCofree
            ( \_ -> Tuple
                ( TriggerSG
                    ( ichange' (Proxy :: _ "sg") (subgraphSingleSetter n (SGWorld false)) *> ichange' (Proxy :: _ "sg2") (subgraphSingleSetter n (SGWorld true))
                    )
                )
                (Identity triggers0)
            )
        )
    )
else instance
  ( Pred n n'
  , Lt n D40
  , CofreeSubgraph0 n'
  ) =>
  CofreeSubgraph0 n where
  cofreeSubgraph0 n = deferCofree
    ( \_ -> Tuple
        ( TriggerSG
            ( ichange' (Proxy :: _ "sg") (subgraphSingleSetter n (SGWorld true)) *> ichange' (Proxy :: _ "sg2") (subgraphSingleSetter n (SGWorld true))
            )
        )
        ( Identity $ deferCofree
            ( \_ -> Tuple
                ( TriggerSG
                    ( ichange' (Proxy :: _ "sg") (subgraphSingleSetter n (SGWorld false)) *> ichange' (Proxy :: _ "sg2") (subgraphSingleSetter n (SGWorld false))
                    )
                )
                (Identity (cofreeSubgraph0 (pred n)))
            )
        )
    )

---

triggers0 :: Cofree Identity TriggerSG
triggers0 = cofreeSubgraph0 d39

subPiece0
  :: forall audio engine
   . AudioInterpret audio engine
  => Int
  -> BrowserAudioBuffer
  -> SubScene "buffy" () SGWorld audio engine Frame0 Unit
subPiece0 _ atar = mempty # SG.loopUsingScene \(SGWorld oo) _ ->
  { control: unit
  , scene:
      { buffy: playBuf
          { onOff: if oo then _on else _off
          }
          atar
      }
  }

subPiece1
  :: forall audio engine
   . AudioInterpret audio engine
  => Int
  -> SubScene "gnn" (beep :: Unit) SGWorld audio engine Frame0 Unit
subPiece1 i = mempty # SG.loopUsingScene \(SGWorld oo) _ ->
  { control: unit
  , scene:
      { gnn: gain
          (if oo then 0.2 else 0.0)
          (input (Proxy :: _ "beep"))
      }
  }

type FullGraph =
  ( speaker :: TSpeaker /\ { gn :: Unit }
  , gn :: TGain /\ { sg :: Unit, sg2 :: Unit }
  , sg :: TSubgraph D40 "buffy" () SGWorld /\ {}
  , sg2 ::
      TSubgraph D40 "gnn" (beep :: Unit) SGWorld
        /\ { beep :: Unit }
  , beep :: TSinOsc /\ {}
  )

createFrame :: BrowserAudioBuffer -> IxWAG RunAudio RunEngine Frame0 Unit () FullGraph Unit
createFrame atar =
  ipatch
    { microphone: Nothing
    , mediaElement: Nothing
    , subgraphs:
        { sg: PatchedSubgraphInput
            { controls: vec
            , scenes: AsSubgraph (\i _ -> subPiece0 i atar)
            , envs: (const $ const $ SGWorld false)
            }
        , sg2: PatchedSubgraphInput
            { controls: vec
            , scenes: AsSubgraph (\i _ -> subPiece1 i)
            , envs: (const $ const $ SGWorld false)
            }
        }
    , tumults: {}
    } :*> ichange' (Proxy :: _ "gn") 1.0
    :*> ichange' (Proxy :: _ "beep") { freq: 550.0, onOff: _on }

piece :: Scene (TriggeredScene Unit World ()) RunAudio RunEngine Frame0 Unit
piece = (\(TriggeredScene env) -> createFrame env.world.atar $> { triggers0 }) @!> iloop \_ acc ->
  (unTriggerSG $ extract acc.triggers0) $> acc { triggers0 = unwrap $ unwrapCofree acc.triggers0 }

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  , freqz :: Array String
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
  , freqz: []
  }

render :: forall m. State -> H.ComponentHTML Action () m
render { freqz } = do
  HH.div_
    $
      [ HH.h1_
          [ HH.text "Patching test" ]
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
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    atar <-
      H.liftAff $ decodeAudioDataFromUri
        audioCtx
        "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop
                ((pure unit) <|> (up $> unit) <|> (down $> unit))
                (pure { atar })
                {}
                (ffiAudio)
                piece
            )
            (\(_ :: TriggeredRun Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
