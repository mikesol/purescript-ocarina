module Main2 where

import Prelude
import Type.Proxy (Proxy(..))

data Yesable

foreign import data Yes :: Yesable

foreign import data Eq' :: Constraint

class UnaryConstraint (c :: Constraint) (t :: Type) (a :: Yesable) | c t -> a

instance unaryConstraintEqYup :: Eq a => UnaryConstraint Eq' a Yes

test0 :: Proxy Yes
test0 = Proxy :: forall (a :: Yesable). UnaryConstraint Eq' Unit a => Proxy a

--test1 :: Proxy Yes
--test1 = Proxy :: forall x y (a :: Yesable). UnaryConstraint Eq' (x -> y) a => Proxy a
gatedIdentity :: forall a. UnaryConstraint Eq' a Yes => a -> a
gatedIdentity = identity

z = gatedIdentity 1 :: Int

--z' = gatedIdentity const
