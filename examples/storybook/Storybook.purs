module WAGS.Example.Storybook where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook, proxy)
import WAGS.Example.AtariSpeaks as AtariSpeaks
import WAGS.Example.DrumMachine as DrumMachine
import WAGS.Example.HelloWorld as HelloWorld
import WAGS.Example.Patching as Patching
import WAGS.Example.KitchenSink as KitchenSink
import WAGS.Example.Makenna as Makenna
import WAGS.Example.NoLoop as NoLoop
import WAGS.Example.SMC2022 as SMC2022
import WAGS.Example.SkipMachine as SkipMachine
import WAGS.Example.Subgraph as Subgraph
import WAGS.Example.Tumult as Tumult
import WAGS.Example.WTK as WTK
import WAGS.Example.WhiteNoise as WhiteNoise

stories :: forall m. MonadEffect m => MonadAff m => MonadThrow Error m => Stories m
stories = Object.fromFoldable
  [ Tuple "atari speaks" $ proxy AtariSpeaks.component
  , Tuple "drum machine" $ proxy DrumMachine.component
  , Tuple "hello world" $ proxy HelloWorld.component
  , Tuple "kitchen sink" $ proxy KitchenSink.component
  , Tuple "happy birthday" $ proxy Makenna.component
  , Tuple "patching" $ proxy Patching.component
  -- media element currently doen't work
  -- needs to be updated to use halogen ref
  -- , Tuple "media element" $ proxy MediaElement.component
  , Tuple "no loop" $ proxy NoLoop.component
  , Tuple "skip machine" $ proxy SkipMachine.component
  , Tuple "smc2022" $ proxy SMC2022.component
  , Tuple "subgraph" $ proxy Subgraph.component
  , Tuple "tumult" $ proxy Tumult.component
  , Tuple "white noise" $ proxy WhiteNoise.component
  , Tuple "wtk" $ proxy WTK.component
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>=
    runStorybook
      { stories
      , logo: Just (HH.text "Wags acceptance tests")  -- pass `Just HH.PlainHTML` to override the logo
      }
