module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.FRP (testFRP)
import Test.Instructions (testInstructions)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Thunkable (testThunkable)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        testThunkable
        testInstructions
        testFRP
