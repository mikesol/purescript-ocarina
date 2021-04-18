module Test.Thunkable where

import Prelude

import Data.Tuple (fst, snd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Control.Thunkable (isHere, runThunkableWithCount, thunkThunkable, wait)

testThunkable :: Spec Unit
testThunkable = do
  describe "thunkable" do
    it "thunks" do
      let
        x = (+) <$> wait 1 <*> pure 2
      let
        x' = runThunkableWithCount x
      (snd x') `shouldEqual` 3
      (fst x') `shouldEqual` 1
      isHere (thunkThunkable x) `shouldEqual` true
      let
        y = wait (+) <*> pure 1 <*> pure 2
      let
        y' = runThunkableWithCount y
      (snd y') `shouldEqual` 3
      (fst y') `shouldEqual` 1
      isHere (thunkThunkable y) `shouldEqual` true
      let
        z = wait (+) <*> wait 1 <*> pure 2
      let
        z' = runThunkableWithCount z
      (snd z') `shouldEqual` 3
      (fst z') `shouldEqual` 2
      isHere (thunkThunkable (thunkThunkable z)) `shouldEqual` true
      let
        a = wait (+) <*> pure 1 <*> wait 2
      let
        a' = runThunkableWithCount a
      (snd a') `shouldEqual` 3
      (fst a') `shouldEqual` 2
      let
        b = (wait (+)) >>= (\f -> pure (f 1)) >>= (\f -> pure (f 2))
      let
        b' = runThunkableWithCount b
      (snd b') `shouldEqual` 3
      (fst b') `shouldEqual` 1
      let
        c = (pure (+)) >>= (\f -> wait (f 1)) >>= (\f -> pure (f 2))
      let
        c' = runThunkableWithCount c
      (snd c') `shouldEqual` 3
      (fst c') `shouldEqual` 1
      let
        d = (pure (+)) >>= (\f -> wait (f 1)) >>= (\f -> wait (f 2))
      let
        d' = runThunkableWithCount d
      (snd d') `shouldEqual` 3
      (fst d') `shouldEqual` 2
