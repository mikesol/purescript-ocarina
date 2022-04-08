module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (cons)
import Data.Filterable (filter)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event (create, subscribe)
import MMZ (fold, mmzToEvent, runMMZ)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
      describe "MMZ" do
        it "should do simple addition" do
          liftEffect do
            rf <- Ref.new []
            unsub <- subscribe (runMMZ (pure 0) \r -> do
              let add1 = map (add 1) r
              mmzToEvent add1) (\i -> Ref.modify_ (cons i) rf)
            o <- liftEffect $ Ref.read rf
            o `shouldEqual` [1]
            unsub
        it "should do more complex addition" do
          liftEffect do
            rf <- Ref.new []
            let e0 /\ e1 = runMMZ (pure 0) \r -> do
                    let add1 = map (add 1) r
                    let add2 = map (add 2) add1
                    let add3 = map (add 3) add2
                    let add4 = map (add 4) add3
                    mmzToEvent add1 /\ mmzToEvent add4
            unsub <- subscribe (e0 <|> e1) (\i -> Ref.modify_ (cons i) rf)
            o <- liftEffect $ Ref.read rf
            o `shouldEqual` [10, 1]
            unsub
        it "should handle alt" do
          liftEffect do
            rf <- Ref.new []
            let e0 /\ e1 = runMMZ (pure 0) \r -> do
                    let add1 = map (add 1) r
                    let add2 = map (add 2) add1
                    let add3 = map (add 3) add2
                    let add4 = map (add 4) add3
                    let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                    mmzToEvent add1 /\ mmzToEvent altr
            unsub <- subscribe (e0 <|> e1) (\i -> Ref.modify_ (cons i) rf)
            o <- liftEffect $ Ref.read rf
            o `shouldEqual` [10,3,1,1]
            unsub
        it "should handle filter 1" do
          liftEffect do
            rf <- Ref.new []
            let e0 /\ e1 = runMMZ (pure 0) \r -> do
                      let add1 = map (add 1) r
                      let add2 = map (add 2) add1
                      let add3 = map (add 3) add2
                      let add4 = map (add 4) add3
                      let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                      let fm = filter (_ < 5) altr
                      mmzToEvent add1 /\ mmzToEvent fm
            unsub <- subscribe (e0 <|> e1) (\i -> Ref.modify_ (cons i) rf)
            o <- liftEffect $ Ref.read rf
            o `shouldEqual` [3,1,1]
            unsub
        it "should handle filter 2" do
          liftEffect do
            rf <- liftEffect $ Ref.new []
            let e0 /\ e1 = runMMZ (pure 0) \r -> do
                  let add1 = map (add 1) r
                  let add2 = map (add 2) add1
                  let add3 = map (add 3) add2
                  let add4 = map (add 4) add3
                  let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
                  let fm = filter (_ > 5) altr
                  mmzToEvent add1 /\ mmzToEvent fm
            unsub <- subscribe (e0 <|> e1) (\i -> Ref.modify_ (cons i) rf)
            o <- liftEffect $ Ref.read rf
            o `shouldEqual` [10,1]
            unsub
        it "should handle fold 0" do
          liftEffect do
            rf <- Ref.new []
            { push, event } <- create
            let e0 /\ e1 = runMMZ event \r -> do
                  let foldy = fold (\_ b -> b + 1) r 0
                  let add2 = map (add 2) foldy
                  let add3 = map (add 3) add2
                  let add4 = map (add 4) add3
                  let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                  let fm = filter (_ > 5) altr
                  mmzToEvent foldy /\ mmzToEvent fm
            unsub <- subscribe (e0 <|> e1) (\i -> Ref.modify_ (cons i) rf)
            push unit
            Ref.read rf >>= shouldEqual [10,1]
            Ref.write [] rf
            push unit
            Ref.read rf >>= shouldEqual [11,2]
            Ref.write [] rf
            push unit
            Ref.read rf >>= shouldEqual [12,3]
            unsub
        it "should handle fold 1" do
          rf <- liftEffect $ Ref.new []
          liftEffect do
            { push, event } <- create
            let e0 = runMMZ event \r -> do
                    let add1 = map (add 1) r
                    let add2 = map (add 2) add1
                    let add3 = map (add 3) add2
                    let foldy = fold (\a b -> a + b) add3 0
                    let add4 = map (add 4) add3
                    let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
                    let fm = Tuple <$> add2 <*> filter (_ > 5) altr
                    mmzToEvent fm
            unsub <- subscribe e0 (\i -> Ref.modify_ (cons i) rf)
            push 0
            Ref.read rf >>= shouldEqual [Tuple 3 10, Tuple 3 6]
            Ref.write [] rf
            push 0
            Ref.read rf >>= shouldEqual [Tuple 3 10, Tuple 3 12]
            Ref.write [] rf
            push 0
            Ref.read rf >>= shouldEqual [Tuple 3 10, Tuple 3 18]
            unsub
