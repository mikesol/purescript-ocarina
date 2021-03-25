module Main7 where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (Except)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.List ((:), List(..))
import Data.List as L
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)

newtype Ctxt :: forall k1 k2. k1 -> k2 -> Type -> Type
newtype Ctxt i o a
  = Ctxt (Except Error a)

derive instance newtypeCtxt :: Newtype (Ctxt i o a) _

derive newtype instance freeProgramFunctor :: Functor (Ctxt i o)

derive newtype instance freeProgramApply :: Apply (Ctxt i o)

derive newtype instance freeProgramBind :: Bind (Ctxt i o)

derive newtype instance freeProgramApplicative :: Applicative (Ctxt i o)

derive newtype instance freeProgramMonad :: Monad (Ctxt i o)

derive newtype instance freeProgramMonadThrow :: MonadThrow Error (Ctxt i o)

instance freeProgramIxFunctor :: IxFunctor Ctxt where
  imap f (Ctxt a) = Ctxt (f <$> a)

instance freeProgramIxApplicative :: IxApply Ctxt where
  iapply (Ctxt f) (Ctxt a) = Ctxt (f <*> a)

instance freeProgramIxApply :: IxApplicative Ctxt where
  ipure a = Ctxt $ pure a

instance freeProgramIxBind :: IxBind Ctxt where
  ibind (Ctxt monad) function = Ctxt (monad >>= (unwrap <<< function))

instance freeProgramIxMonad :: IxMonad Ctxt

data Nat'

foreign import data After' :: Nat' -> Nat'

foreign import data Zero' :: Nat'

data Unk'

foreign import data UnkX' :: Unk' -> Unk'

foreign import data Unk0' :: Unk'

data Expr'

foreign import data Const' :: Nat' -> Expr'

foreign import data Var' :: Unk' -> Expr'

foreign import data Plus' :: Expr' -> Expr' -> Expr'

data Nat
  = After Nat
  | Zero

newtype Vec (n :: Expr') a
  = Vec (List a)

asVec :: forall a m (i :: Unk'). Applicative (m i (UnkX' i)) => List a -> m i (UnkX' i)  (Vec (Var' i) a)
asVec = pure <<< Vec


assertEq :: forall a err m (i :: Unk') (j :: Expr') (k :: Expr'). MonadThrow err (m i (UnkX' i)) => IxMonad m => err -> Vec j a -> Vec k a -> m i (UnkX' i) (Tuple (Vec (Var' i) a) (Vec (Var' i) a))
assertEq err (Vec a) (Vec b)
  | L.length a /= L.length b = pure $ Tuple (Vec a) (Vec b)
  | otherwise = throwError err

consVec :: forall (a :: Expr') x. x -> Vec a x -> Vec (Plus' (Const' (After' Zero')) a) x
consVec a (Vec x) = Vec (Cons a x)

infixr 4 consVec as +>

appendVec :: forall (a :: Expr') (b :: Expr') x. Vec a x -> Vec b x -> Vec (Plus' a b) x
appendVec (Vec x) (Vec y) = Vec (x <> x)

infixr 4 appendVec as <+>

nil :: forall x. Vec (Const' Zero') x
nil = Vec Nil

class LNat (a :: Nat') (b :: Nat') (c :: Nat') (d :: Nat') | a b -> c d

instance lNatA :: LNat Zero' Zero' Zero' Zero'

instance lNatB :: LNat (After' x) Zero' Zero' (After' x)

instance lNatC :: LNat Zero' (After' x) Zero' (After' x)

instance lNatD :: LNat a b x y => LNat (After' a) (After' b) x y

class LUnk (a :: Unk') (b :: Unk') (c :: Unk') (d :: Unk') | a b -> c d

instance lUnkA :: LUnk Unk0' Unk0' Unk0' Unk0'

instance lUnkB :: LUnk (UnkX' x) Unk0' Unk0' (UnkX' x)

instance lUnkC :: LUnk Unk0' (UnkX' x) Unk0' (UnkX' x)

instance lUnkD :: LUnk a b x y => LUnk (UnkX' a) (UnkX' b) x y

class Leftmost (a :: Expr') (b :: Expr') (c :: Expr') (d :: Expr') | a b -> c d

instance leftmostLeftmostA :: Leftmost (Const' a) (Var' b) (Var' b) (Const' a)

instance leftmostLeftmostB :: Leftmost (Var' b) (Const' a) (Var' b) (Const' a)

instance leftmostLeftmostC :: LNat a b x y => Leftmost (Const' a) (Const' b) (Const' x) (Const' y)

instance leftmostLeftmostD :: LUnk a b x y => Leftmost (Var' b) (Var' a) (Var' x) (Var' y)

class BalanceExpr (a :: Expr') (b :: Expr') | a -> b

instance balanceExprConst :: BalanceExpr (Const' a) (Const' a)

instance balanceExprVar :: BalanceExpr (Var' a) (Var' a)

instance balanceExprPlus ::
  ( BalanceExpr (Plus' m n) (Plus' a' a'')
  , BalanceExpr (Plus' o p) (Plus' b' b'')
  , Leftmost a' b' x y
  , BalanceExpr (Plus' y (Plus' a'' b'')) z
  ) =>
  BalanceExpr (Plus' (Plus' m n) (Plus' o p)) (Plus' x z)

instance balanceExprConstL ::
  ( BalanceExpr (Plus' o p) (Plus' a' a'')
  , Leftmost (Const' a) a' x y
  , BalanceExpr (Plus' y a'') z
  ) =>
  BalanceExpr (Plus' (Const' a) (Plus' o p)) (Plus' x z)

instance balanceExprVarL ::
  ( BalanceExpr (Plus' o p) (Plus' a' a'')
  , Leftmost (Var' a) a' x y
  , BalanceExpr (Plus' y a'') z
  ) =>
  BalanceExpr (Plus' (Var' a) (Plus' o p)) (Plus' x z)

instance balanceExprConstR ::
  ( BalanceExpr (Plus' o p) (Plus' a' a'')
  , Leftmost (Const' a) a' x y
  , BalanceExpr (Plus' y a'') z
  ) =>
  BalanceExpr (Plus' (Plus' o p) (Const' a)) (Plus' x z)

instance balanceExprVarR ::
  ( BalanceExpr (Plus' o p) (Plus' a' a'')
  , Leftmost (Var' a) a' x y
  , BalanceExpr (Plus' y a'') z
  ) =>
  BalanceExpr (Plus' (Plus' o p) (Var' a)) (Plus' x z)

instance balanceExprVarVar ::
  ( Leftmost (Var' a) (Var' b) x y
    ) =>
  BalanceExpr (Plus' (Var' a) (Var' b)) (Plus' x y)

instance balanceExprConstConst ::
  ( Leftmost (Const' a) (Const' b) x y
    ) =>
  BalanceExpr (Plus' (Const' a) (Const' b)) (Plus' x y)

instance balanceExprConstVar ::
  BalanceExpr (Plus' (Const' a) (Var' b)) (Plus' (Var' b) (Const' a))

instance balanceExprVarConst ::
  BalanceExpr (Plus' (Var' b) (Const' a)) (Plus' (Var' b) (Const' a))

class EqExpr (a :: Expr') (b :: Expr')

instance eqExpr :: (BalanceExpr a c, BalanceExpr b c) => EqExpr a b

zipWith :: forall (a :: Expr') (b :: Expr') x y z. EqExpr a b => (x -> y -> z) -> Vec a x -> Vec b y -> Vec a z
zipWith f (Vec a) (Vec b) = Vec (L.zipWith f a b)

-----------------

-- compiles because we have asserted equality
test0 list0 list1 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- asVec list0
  v1 <- asVec list1
  (Tuple l r) <- assertEq (error "not eq") v0 v1
  ipure $ zipWith (+) l r

-----
-- doesn't compile because we haven't asserted equality
{-
test1 list0 list1 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- asVec list0
  v1 <- asVec list1
  ipure $ zipWith (+) list0 list1
-}

-- compiles because the vec cons operation preserves equality
test2 list0 list1 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- asVec list0
  v1 <- asVec list1
  (Tuple l r) <- assertEq (error "not eq") v0 v1
  ipure $ zipWith (+) (1 +> l) (5 +> r)

-- compiles because the vec append operation preserves equality
test3 list0 list1 list2 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- asVec list0
  v1 <- asVec list1
  v2 <- asVec list2
  (Tuple l r) <- assertEq (error "not eq") v0 v1
  ipure $ zipWith (+) (v2 <+> l) (v2 <+> r)

-- does not compile because the cons operation was only applied to l, not to r
{-
test4 list0 list1 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- asVec list0
  v1 <- asVec list1
  (Tuple l r) <- assertEq (error "not eq") v0 v1
  ipure $ zipWith (+) (1 +> l) r
-}

-- does not compile because the append operation was only applied to l, not to r
{-
test5 list0 list1 list2 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- asVec list0
  v1 <- asVec list1
  v2 <- asVec list2
  (Tuple l r) <- assertEq (error "not eq") v0 v1
  ipure $ zipWith (+) (v2 <+> l) r
-}

test6 list0 list1 list2 list3 = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- asVec list0
  v1 <- asVec list1
  v2 <- asVec list2
  v3 <- asVec list3
  (Tuple l r) <- assertEq (error "not eq") v0 v1
  (Tuple x y) <- assertEq (error "not eq") v2 v3
  ipure $ zipWith (+) (l <+> y) (x <+> r)
