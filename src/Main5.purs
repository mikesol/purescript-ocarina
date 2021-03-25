module Main5 where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error)

data Nat
  = After Nat
  | Zero

data Cons a x
  = Cons a x

infixr 4 Cons as :

data Nil
  = Nil

class List :: forall k. k -> Constraint
class List a

derive instance gCons :: Generic (Cons a x) _

instance sCons :: (Show a, Show x) => Show (Cons a x) where
  show s = genericShow s

derive instance gNil :: Generic Nil _

instance sNil :: Show Nil where
  show s = genericShow s

instance listNil :: List Nil

instance listCons :: List x => List (Cons a x)

class Zip fa fb fc a b c | fa -> a, fb -> b, fc -> c, fa fb -> fc where
  zip :: (a -> b -> c) -> fa -> fb -> fc

instance zipNil :: Zip Nil Nil Nil a b c where
  zip _ _ _ = Nil

instance zipCons :: Zip a' b' c' a b c => Zip (Cons a a') (Cons b b') (Cons c c') a b c where
  zip f (Cons a a') (Cons b b') = Cons (f a b) (zip f a' b')

class Fill fa a | fa -> a where
  fill :: a -> fa

instance fillNil :: Fill Nil a where
  fill _ = Nil

instance fillCons :: (Fill x a) => Fill (Cons a x) a where
  fill i = Cons i (fill i)

toListEffect :: ∀ (a ∷ Type) (l ∷ Type) (m ∷ Type -> Type). ToList a l ⇒ MonadThrow Error m ⇒ Array a → m l
toListEffect = toList (error "List not of right length")


instance semiringNil :: Semiring Nil where
  add _ _ = Nil
  mul _ _ = Nil
  zero = Nil
  one = Nil

instance semiringCons :: (Semiring a, Semiring x, Fill (Cons a x) a, Zip (Cons a x) (Cons a x) (Cons a x) a a a) => Semiring (Cons a x) where
  add = zip (+)
  mul = zip (*)
  zero = fill zero
  one = fill one

logMe :: ∀ (m ∷ Type -> Type) (a ∷ Type). MonadEffect m ⇒ Show a ⇒ a → m Unit
logMe = log <<< show

-- eliminate the existential type by providing a context where it can be discharged

class ToList a o | o -> a where
  toList :: forall e m. MonadThrow e m => e -> Array a -> m o

instance toListNil :: ToList a Nil where
  toList _ [] = pure Nil
  toList e _ = throwError e

instance toListCons :: ToList a x => ToList a (Cons a x) where
  toList e a = case A.head a of
    Nothing -> throwError e
    Just x -> Cons x <$> toList e (A.drop 1 a)

program :: forall l. Semiring l => Show l => Zip l l l Int Int Int => ToList Int l => Array Int -> Array Int -> Effect Unit
program a b = do
  l0 :: l <- toListEffect a
  l1 :: l <- toListEffect b
  logMe $ l0 + l1




main :: Effect Unit
main = do
  logMe (fill 6 :: (Cons Int (Cons Int Nil)) )
  logMe Nil
  logMe $ zip (+) (1 : Nil) (2 : Nil)
  logMe $ zip (+) (1 : 3 : Nil) (2 : 6 : Nil)
  let q = zip (+) (1 : 3 :  Nil) (2 : 6 : Nil)
  logMe q
  logMe $ (1 : 3 : Nil) + (4 : 5 : Nil)
  logMe $ (44 : 46 : Nil) + (fill 6)
  logMe $ (44 : 46 : Nil) * zero
  --l0  <- toListEffect [1,2] :: forall l. ToList Int l => Effect l
  --l1 <- toListEffect [1,2]
  --logMe $ l0 + l1
  pure unit
