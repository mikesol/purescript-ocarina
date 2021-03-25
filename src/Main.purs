module Main where

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

toInt :: Nat -> Int
toInt Zero = 0

toInt (After n) = 1 + toInt n

toNat :: forall e m. MonadThrow e m => e -> Int -> m Nat
toNat e x
  | x < 0 = throwError e
  | x == 0 = pure Zero
  | otherwise = After <$> toNat e (x - 1)

derive instance genericNat :: Generic Nat _

instance showNat :: Show Nat where
  show s = genericShow s

add' :: Nat -> Nat -> Nat
add' Zero x = x

add' x Zero = x

add' (After x) (After y) = After (After (add' x y))

mul' :: Nat -> Nat -> Nat
mul' Zero x = Zero

mul' x Zero = Zero

mul' (After x) y = add' y (mul' x y)

toNatEffect :: ∀ (m ∷ Type -> Type). MonadThrow Error m ⇒ Int → m Nat
toNatEffect = toNat (error "Natural numbers must be gteq 0")


instance semiringNat :: Semiring Nat where
  add = add'
  mul = mul'
  zero = Zero
  one = After Zero

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

main :: Effect Unit
main = do
  {-logMe Zero
  logMe (After Zero)
  logMe (After (After Zero))
  logMe $ toInt (After (After Zero))
  twenty <- toNatEffect 20
  twenty'one <- toNatEffect 21
  logMe $ toInt $ twenty + twenty'one
  logMe $ toInt $ twenty * twenty'one
  logMe (fill 6 :: (Cons Int (Cons Int Nil)) )
  logMe Nil
  logMe $ zip (+) (1 : Nil) (2 : Nil)
  logMe $ zip (+) (1 : 3 : Nil) (2 : 6 : Nil)
  let q = zip (+) (1 : 3 :  Nil) (2 : 6 : Nil)
  logMe q-}
  logMe $ (1 : 3 : 10 : Nil) + (4 : 5 : 11 : Nil)
{-  logMe $ (44 : 46 : Nil) + (fill 6)
  logMe $ (44 : 46 : Nil) * zero
  pure unit-}
