module WAGS.Universe.Bin where

import Prelude
import Data.Int.Bits (shl)
import Data.Typelevel.Bool (class And, False, True)
import Type.Proxy (Proxy(..))
import WAGS.Util (class Gate)

type D0
  = (Bc O Bn)

type D1
  = (Bc I Bn)

type D2
  = (Bc O (Bc I Bn))

type D3
  = (Bc I (Bc I Bn))

type D4
  = (Bc O (Bc O (Bc I Bn)))

type D5
  = (Bc I (Bc O (Bc I Bn)))

type D6
  = (Bc O (Bc I (Bc I Bn)))

type D7
  = (Bc I (Bc I (Bc I Bn)))

data Bin

foreign import data I :: Bin

foreign import data O :: Bin

data BinL

foreign import data Bc :: Bin -> BinL -> BinL

foreign import data Bn :: BinL

type Ptr
  = BinL

data PtrList

foreign import data PtrListCons :: Ptr -> PtrList -> PtrList

foreign import data PtrListNil :: PtrList

class BinToInt (i :: BinL) where
  toInt'' :: Int -> Proxy i -> Int

instance toIntBn :: BinToInt Bn where
  toInt'' _ _ = 0

instance toIntBcO :: BinToInt r => BinToInt (Bc O r) where
  toInt'' x _ = toInt'' (x `shl` 1) (Proxy :: _ r)

instance toIntBcI :: BinToInt r => BinToInt (Bc I r) where
  toInt'' x _ = x + toInt'' (x `shl` 1) (Proxy :: _ r)

toInt' :: forall (i :: BinL). BinToInt i => Proxy i -> Int
toInt' = toInt'' 1

class BinSucc (i :: BinL) (o :: BinL) | i -> o

instance binSuccNull :: BinSucc Bn (Bc I Bn)

instance binSuccO :: BinSucc (Bc O r) (Bc I r)

instance binSuccI :: BinSucc r r' => BinSucc (Bc I r) (Bc O r')

class BinSub' (carrying :: Type) (l :: BinL) (r :: BinL) (o :: BinL) | carrying l r -> o

instance binSubDoneIF :: BinSub' False (Bc I r) Bn (Bc I r)

instance binSubDoneIT :: BinSub' True (Bc I r) Bn (Bc O r)

instance binSubDoneOF :: BinSub' False (Bc O r) Bn (Bc O r)

instance binSubDoneOT :: BinSub' True r Bn o => BinSub' True (Bc O r) Bn o

instance binSubNul1 :: BinSub' True Bn (Bc x y) Bn

instance binSubNul2 :: BinSub' True Bn Bn Bn

instance binSubNul3 :: BinSub' False Bn (Bc x y) Bn

instance binSubNul4 :: BinSub' False Bn Bn Bn

instance binSubIterFOO :: BinSub' False i o r => BinSub' False (Bc O i) (Bc O o) (Bc O r)

instance binSubIterFIO :: BinSub' False i o r => BinSub' False (Bc I i) (Bc O o) (Bc I r)

instance binSubIterFOI :: BinSub' True i o r => BinSub' False (Bc O i) (Bc I o) (Bc I r)

instance binSubIterFII :: BinSub' False i o r => BinSub' False (Bc I i) (Bc I o) (Bc O r)

----------
instance binSubIterTOO :: BinSub' False (Bc O i) (Bc I o) x => BinSub' True (Bc O i) (Bc O o) x

instance binSubIterTIO :: BinSub' False (Bc I i) (Bc I o) x => BinSub' True (Bc I i) (Bc O o) x

instance binSubIterTOI :: BinSub' True i o r => BinSub' True (Bc O i) (Bc I o) (Bc O r)

instance binSubIterTII :: BinSub' False (Bc O i) (Bc I o) x => BinSub' True (Bc I i) (Bc I o) x

class Beq (a :: Bin) (b :: Bin) (tf :: Type) | a b -> tf

instance beqOO :: Beq O O True

instance beqOI :: Beq O I False

instance beqIO :: Beq I O False

instance beqII :: Beq I I True

class BinEq (a :: BinL) (b :: BinL) (tf :: Type) | a b -> tf

instance binEq0 :: BinEq Bn Bn True

instance binEq1 :: BinEq Bn (Bc x y) False

instance binEq2 :: BinEq (Bc x y) Bn False

instance binEq3 :: (Beq a x tf, BinEq b y rest, And tf rest r) => BinEq (Bc a b) (Bc x y) r

class AllZerosToNull (i :: BinL) (o :: BinL) | i -> o

instance allZerosToNullBn :: AllZerosToNull Bn Bn

instance allZerosToNullBcI :: AllZerosToNull (Bc I o) (Bc I o)

instance allZerosToNullBcO :: AllZerosToNull o x => AllZerosToNull (Bc O o) x

class RemoveTrailingZeros (i :: BinL) (o :: BinL) | i -> o

instance removeTrailingZerosBn :: RemoveTrailingZeros Bn Bn

instance removeTrailingZerosI :: RemoveTrailingZeros r r' => RemoveTrailingZeros (Bc I r) (Bc I r')

instance removeTrailingZerosO ::
  ( AllZerosToNull r n
  , RemoveTrailingZeros r r'
  , BinEq n Bn tf
  , Gate tf Bn (Bc O r') x
  ) =>
  RemoveTrailingZeros (Bc O r) x

class BinSub (l :: BinL) (r :: BinL) (o :: BinL) | l r -> o

instance binSub :: (BinSub' False l r o', RemoveTrailingZeros o' o) => BinSub l r o

infixr 5 type PtrListCons as +:


class PtrListKeepSingleton (ptrListA :: PtrList) (ptrListB :: PtrList) (ptrListC :: PtrList) | ptrListA ptrListB -> ptrListC

instance ptrListKeepSingletonNil :: PtrListKeepSingleton PtrListNil PtrListNil PtrListNil

instance ptrListKeepSingletonL :: PtrListKeepSingleton (PtrListCons a PtrListNil) PtrListNil (PtrListCons a PtrListNil)

instance ptrListKeepSingletonR :: PtrListKeepSingleton PtrListNil (PtrListCons a PtrListNil) (PtrListCons a PtrListNil)