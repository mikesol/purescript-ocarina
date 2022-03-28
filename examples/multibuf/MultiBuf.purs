module WAGS.Example.MultiBuf where

-- should continue scheduling things at 0.25s intervals ad infinitum

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree, mkCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as Array
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Identity (Identity(..))
import Data.Map (empty, fromFoldable, insert, toUnfoldable, union)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (swap)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D40)
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
import WAGS.Graph.Parameter (AudioOnOff(..), _on)
import WAGS.Interpret (class AudioInterpret, close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Run (RunAudio, RunEngine, TriggeredRun, TriggeredScene(..), runNoLoop)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World = { kick :: BrowserAudioBuffer, snare :: BrowserAudioBuffer }

newtype SGWorld = SGWorld Number

subPiece0
  :: forall audio engine
   . AudioInterpret audio engine
  => { kick :: BrowserAudioBuffer, snare :: BrowserAudioBuffer }
  -> Int
  -> SubScene "buffy" () SGWorld audio engine Frame0 Unit
subPiece0 { kick, snare } i = mempty # SG.loopUsingScene \(SGWorld time) _ ->
  { control: unit
  , scene:
      { buffy: playBuf
          { onOff: AudioOnOff { onOff: _on, timeOffset: time } }
          (if i `mod` 2 == 0 then kick else snare)
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
            (f (i + 1) (x + 0.25))

        in
          { cf: f 0 0.0, prevs: empty }
  ) # loopUsingScene \(TriggeredScene env) { cf, prevs } ->
    let
      { head, rest } = unrollCofree 20 cf
      -- (i → b → a → b) → b → f a → b
      { yes, no } = foldlWithIndex
        ( \k yn v ->
            if k < (env.time - 2.0) then yn { no = insert k v yn.no }
            else yn { yes = insert k v yn.yes }
        )
        { yes: empty, no: empty }
        prevs
    in
      { control: { cf: rest, prevs: union yes $ fromFoldable (map swap head) }
      , scene: speaker
          let
            envs = fromFoldable
              ( ( map
                    ( \(x /\ y) -> x /\
                        (just $ SGWorld (max 0.0 (y - env.time)))
                    )
                    head
                ) <> (map <<< map) (const nothing) (map swap (toUnfoldable no))
              )
          in
            { gn: gain 1.0
                { sg: subgraph envs
                    (subPiece0 { kick: env.world.kick, snare: env.world.snare })
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
  , freqz :: Array String
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
  , freqz: []
  }

render :: forall m. State -> H.ComponentHTML Action () m
render { freqz } = do
  HH.div_
    $
      [ HH.h1_
          [ HH.text "MultiBuf test" ]
      , HH.button
          [ HE.onClick \_ -> StartAudio ]
          [ HH.text "Start audio" ]
      , HH.button
          [ HE.onClick \_ -> StopAudio ]
          [ HH.text "Stop audio" ]
      ]
        <> map (\freq -> HH.p [] [ HH.text freq ]) freqz

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
    kick <-
      H.liftAff $ decodeAudioDataFromUri
        audioCtx
        "https://freesound.org/data/previews/344/344757_1676145-hq.mp3"
    snare <- H.liftAff $ decodeAudioDataFromUri
      audioCtx
      "https://freesound.org/data/previews/387/387186_7255534-hq.mp3"
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop (pure unit <|> (interval 4900 $> unit))
                (pure { kick, snare })
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
