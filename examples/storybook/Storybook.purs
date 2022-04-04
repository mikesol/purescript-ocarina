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
import WAGS.Example.HelloWorld as DrumMachine
import WAGS.Example.HelloWorld as HelloWorld
import WAGS.Example.Subgraph as Subgraph
import WAGS.Example.WhiteNoise as WhiteNoise

-- stories :: forall m. MonadEffect m => MonadAff m => MonadThrow Error m => Stories m
-- stories = Object.fromFoldable
--   [ Tuple "atari speaks" $ proxy AtariSpeaks.component
--   , Tuple "drum machine" $ proxy DrumMachine.component
--   , Tuple "hello world" $ proxy HelloWorld.component
--   , Tuple "subgraph" $ proxy Subgraph.component
--   , Tuple "white noise" $ proxy WhiteNoise.component
--   ]

-- main :: Effect Unit
-- main = HA.runHalogenAff do
--   HA.awaitBody >>=
--     runStorybook
--       { stories
--       , logo: Just (HH.text "Wags acceptance tests")  -- pass `Just HH.PlainHTML` to override the logo
--       }
