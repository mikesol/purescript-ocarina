module Test.FRP where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Foldable (for_)
import Data.List (List(..), length)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event (EventIO, create, subscribe)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS (bufferToList)

testFRP :: Spec Unit
testFRP = do
  describe "delayEvent" do
    it "respects the delay" do
      stash <- liftEffect $ Ref.new (Nil :: List (List { time :: Instant, value :: Unit }))
      (myEvent :: EventIO Unit) <- liftEffect create
      liftEffect
        $ launchAff_ do
            for_ [ 0, 1, 2, 3 ] \_ -> do
              delay (Milliseconds 50.0)
              for_ [ 0, 1, 2, 3 ] \_ -> do
                liftEffect $ myEvent.push unit
                delay (Milliseconds 1.0)
      canceler <-
        liftEffect
          $ subscribe (bufferToList 10 myEvent.event) \i -> do
              Ref.modify_ (Cons i) stash
      delay (Milliseconds 300.0)
      incoming <- liftEffect $ Ref.read stash
      length incoming `shouldEqual` 4
      for_ incoming (shouldEqual 4 <<< length)
      liftEffect $ canceler
