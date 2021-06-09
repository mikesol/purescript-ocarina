module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.FRP (testFRP)
import Test.Instructions (testInstructions)
import Test.NE2CF (testNE2CF)
import Test.Patch (testPatch)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        testInstructions
        testFRP
        testPatch
        testNE2CF
