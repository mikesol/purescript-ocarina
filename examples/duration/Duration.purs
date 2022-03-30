module WAGS.Example.Duration where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as Array
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (swap)
import Data.Tuple.Nested ((/\))
import Data.Variant.Maybe (just, nothing)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import FRP.Event.Time (interval)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import WAGS.Control.Functions.Graph (loopUsingScene)
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Types (Frame0, Scene, SubScene)
import WAGS.Create.Optionals (gain, playBuf, speaker, subgraph)
import WAGS.Interpret (class AudioInterpret, close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Graph.Parameter (AudioOnOff(..), _on)
import WAGS.Run (TriggeredRun, RunAudio, RunEngine, TriggeredScene(..), runNoLoop)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World =
  { sound :: BrowserAudioBuffer
  }

newtype SGWorld = SGWorld Number

subPiece0
  :: forall audio engine
   . AudioInterpret audio engine
  => { sound :: BrowserAudioBuffer }
  -> Int
  -> SubScene "buffy" () SGWorld audio engine Frame0 Unit
subPiece0 { sound } _ = mempty # SG.loopUsingScene \(SGWorld time) _ ->
  { control: unit
  , scene:
      { buffy: playBuf
          { onOff: AudioOnOff { onOff: _on, timeOffset: time }, duration: 2.5 }
          sound
      }
  }

unrollCofree
  :: forall a
   . Int
  -> Cofree Identity a
  -> { head :: Array a, rest :: Cofree Identity a }
unrollCofree 0 cf = { head: [], rest: cf }
unrollCofree n cf =
  let
    h = extract cf
    t = unwrap $ unwrapCofree cf
    v = unrollCofree (n - 1) t
  in
    { head: Array.cons h v.head, rest: v.rest }

piece :: Scene (TriggeredScene Unit World ()) RunAudio RunEngine Frame0 Unit
piece =
  ( const
      $
        let
          f i x = deferCofree \_ -> (i /\ x) /\ Identity
            (f (i + 1) (x + 2.5))

        in
          { cf: f 0 0.0, prevs: Map.empty }
  ) # loopUsingScene \(TriggeredScene { time, world }) { cf, prevs } ->
    let
      { head, rest } = unrollCofree 20 cf
      -- (i → b → a → b) → b → f a → b
      { yes, no } = foldlWithIndex
        ( \k yn v ->
            if k < (time - 2.0) then yn { no = Map.insert k v yn.no }
            else yn { yes = Map.insert k v yn.yes }
        )
        { yes: Map.empty, no: Map.empty }
        prevs
    in
      { control:
          { cf: rest, prevs: Map.union yes $ Map.fromFoldable (map swap head) }
      , scene: speaker
          let
            envs = Map.fromFoldable
              ( ( map
                    ( \(x /\ y) -> x /\
                        (just $ SGWorld (max 0.0 (y - time)))
                    )
                    head
                ) <> (map <<< map) (const nothing)
                  (map swap (Map.toUnfoldable no))
              )
          in
            { gn: gain 1.0
                { sg: subgraph envs
                    (subPiece0 world)
                    {}
                }
            }
      }

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
          [ HH.text "Duration" ]
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
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    sound <-
      H.liftAff $ decodeAudioDataFromUri
        audioCtx
        "https://freesound.org/data/previews/218/218558_468390-hq.mp3"
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop (pure unit <|> (interval 4900 $> unit)) (pure { sound })
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
