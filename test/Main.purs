module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (cons)
import Data.Filterable (filter)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import MMZ (addSubscription, encapsulateMMZ, fold, runMMZ)
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
          rf <- liftEffect $ Ref.new []
          liftEffect do
            encapsulateMMZ \p r -> do
              let add1 = map (add 1) r
              addSubscription (\i -> Ref.modify_ (cons i) rf) add1
              runMMZ p 0 add1
          o <- liftEffect $ Ref.read rf
          o `shouldEqual` [1]
        it "should do more complex addition" do
          rf <- liftEffect $ Ref.new []
          liftEffect do
            encapsulateMMZ \p r -> do
              let add1 = map (add 1) r
              let add2 = map (add 2) add1
              let add3 = map (add 3) add2
              let add4 = map (add 4) add3
              addSubscription (\i -> Ref.modify_ (cons i) rf) add1
              addSubscription (\i -> Ref.modify_ (cons i) rf) add4
              runMMZ p 0 add4
          o <- liftEffect $ Ref.read rf
          o `shouldEqual` [10, 1]
        it "should handle alt" do
          rf <- liftEffect $ Ref.new []
          liftEffect do
            encapsulateMMZ \p r -> do
              let add1 = map (add 1) r
              let add2 = map (add 2) add1
              let add3 = map (add 3) add2
              let add4 = map (add 4) add3
              let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
              addSubscription (\i -> Ref.modify_ (cons i) rf) add1
              addSubscription (\i -> Ref.modify_ (cons i) rf) altr
              runMMZ p 0 altr
          o <- liftEffect $ Ref.read rf
          o `shouldEqual` [10,3,1,1]
        it "should handle filter 1" do
          rf <- liftEffect $ Ref.new []
          liftEffect do
            encapsulateMMZ \p r -> do
              let add1 = map (add 1) r
              let add2 = map (add 2) add1
              let add3 = map (add 3) add2
              let add4 = map (add 4) add3
              let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
              let fm = filter (_ < 5) altr
              addSubscription (\i -> Ref.modify_ (cons i) rf) add1
              addSubscription (\i -> Ref.modify_ (cons i) rf) fm
              runMMZ p 0 altr
          o <- liftEffect $ Ref.read rf
          o `shouldEqual` [3,1,1]
        it "should handle filter 2" do
          rf <- liftEffect $ Ref.new []
          liftEffect do
            encapsulateMMZ \p r -> do
              let add1 = map (add 1) r
              let add2 = map (add 2) add1
              let add3 = map (add 3) add2
              let add4 = map (add 4) add3
              let altr = add1 <|> add2 <|> empty <|> add4 <|> empty
              let fm = filter (_ > 5) altr
              addSubscription (\i -> Ref.modify_ (cons i) rf) add1
              addSubscription (\i -> Ref.modify_ (cons i) rf) fm
              runMMZ p 0 altr
          o <- liftEffect $ Ref.read rf
          o `shouldEqual` [10,1]
        it "should handle fold 0" do
          rf <- liftEffect $ Ref.new []
          liftEffect do
            encapsulateMMZ \p r -> do
              let foldy = fold (\_ b -> b + 1) r 0
              let add2 = map (add 2) foldy
              let add3 = map (add 3) add2
              let add4 = map (add 4) add3
              let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
              let fm = filter (_ > 5) altr
              addSubscription (\i -> Ref.modify_ (cons i) rf) foldy
              addSubscription (\i -> Ref.modify_ (cons i) rf) fm
              runMMZ p unit altr
              Ref.read rf >>= shouldEqual [10,1]
              Ref.write [] rf
              runMMZ p unit altr
              Ref.read rf >>= shouldEqual [11,2]
              Ref.write [] rf
              runMMZ p unit altr
              Ref.read rf >>= shouldEqual [12,3]
        it "should handle fold 1" do
          rf <- liftEffect $ Ref.new []
          liftEffect do
            encapsulateMMZ \p r -> do
              let add1 = map (add 1) r
              let add2 = map (add 2) add1
              let add3 = map (add 3) add2
              let foldy = fold (\a b -> a + b) add3 0
              let add4 = map (add 4) add3
              let altr = foldy <|> add2 <|> empty <|> add4 <|> empty
              let fm = Tuple <$> add2 <*> filter (_ > 5) altr
              addSubscription (\i -> Ref.modify_ (cons i) rf) fm
              runMMZ p 0 altr
              Ref.read rf >>= shouldEqual [Tuple 3 10, Tuple 3 6]
              Ref.write [] rf
              runMMZ p 0 altr
              Ref.read rf >>= shouldEqual [Tuple 3 10, Tuple 3 12]
              Ref.write [] rf
              runMMZ p 0 altr
              Ref.read rf >>= shouldEqual [Tuple 3 10, Tuple 3 18]
