module WAGS.Universe.Bin where

import Prelude
import Data.Int.Bits (shl)
import Data.Typelevel.Bool (class And, False, True)
import Type.Proxy (Proxy(..))
import WAGS.Util (class Gate)

-- | A single bit
data Bit

-- | An on bit
foreign import data I :: Bit

-- | An off bit
foreign import data O :: Bit

-- | 0 or more bits. Note that, in all of the class operations on `Bits`, the lowest bit is in the
-- | leftmost position and the calculation proceeds from left to right. This is the opposite of how
-- | bits are usually represented (lowest on the right and highest on the left) but is more compuationally efficient.
data Bits

-- | Cons for bits
foreign import data Bc :: Bit -> Bits -> Bits

-- | Nil for bits
foreign import data Bn :: Bits

-- | Ptr is an alias for bits
type Ptr
  = Bits

-- | A list of pointers
data PtrList

-- | Cons for a pointer list
foreign import data PtrListCons :: Ptr -> PtrList -> PtrList

-- | Nil for a pointer list
foreign import data PtrListNil :: PtrList

-- Cast a binary value to an int
toInt' :: forall (i :: Bits). BinToInt i => Proxy i -> Int
toInt' = toInt'' 1

-- Cast a binary value to an int
toInt :: forall (i :: Bits). BinToInt i => Int
toInt = toInt'' 1 (Proxy :: _ i)

-- Class representing the successor function for binary values.
class BinSucc (i :: Bits) (o :: Bits) | i -> o

instance binSuccNull :: BinSucc Bn (Bc I Bn)

instance binSuccO :: BinSucc (Bc O r) (Bc I r)

instance binSuccI :: BinSucc r r' => BinSucc (Bc I r) (Bc O r')

-- Class representing subtraction of binary values if l is greater than r and Bn otherwise.
class BinSub (l :: Bits) (r :: Bits) (o :: Bits) | l r -> o

instance binSub :: (BinSub' False l r o', RemoveTrailingZeros o' o) => BinSub l r o

-- Internal class helping with binary subtraction.
class BinSub' (carrying :: Type) (l :: Bits) (r :: Bits) (o :: Bits) | carrying l r -> o

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

instance binSubIterTOO :: BinSub' False (Bc O i) (Bc I o) x => BinSub' True (Bc O i) (Bc O o) x

instance binSubIterTIO :: BinSub' False (Bc I i) (Bc I o) x => BinSub' True (Bc I i) (Bc O o) x

instance binSubIterTOI :: BinSub' True i o r => BinSub' True (Bc O i) (Bc I o) (Bc O r)

instance binSubIterTII :: BinSub' False (Bc O i) (Bc I o) x => BinSub' True (Bc I i) (Bc I o) x

-- Class asking if two bits are equal and responding t if true and f otherwise, aka NOT XOR.
class Beq (a :: Bit) (b :: Bit) (tf :: Type) | a b -> tf

instance beqOO :: Beq O O True

instance beqOI :: Beq O I False

instance beqIO :: Beq I O False

instance beqII :: Beq I I True

-- Class asking if two binary values are equal and responding t if true and f otherwise.
class BinEq (a :: Bits) (b :: Bits) (tf :: Type) | a b -> tf

instance binEq0 :: BinEq Bn Bn True

instance binEq1 :: BinEq Bn (Bc x y) False

instance binEq2 :: BinEq (Bc x y) Bn False

instance binEq3 :: (Beq a x tf, BinEq b y rest, And tf rest r) => BinEq (Bc a b) (Bc x y) r

-- Class that turns a list of Os to `Bn`.
class AllZerosToNull (i :: Bits) (o :: Bits) | i -> o

instance allZerosToNullBn :: AllZerosToNull Bn Bn

instance allZerosToNullBcI :: AllZerosToNull (Bc I o) (Bc I o)

instance allZerosToNullBcO :: AllZerosToNull o x => AllZerosToNull (Bc O o) x

-- Class that removes trailing 0s from a binary number.
class RemoveTrailingZeros (i :: Bits) (o :: Bits) | i -> o

instance removeTrailingZerosBn :: RemoveTrailingZeros Bn Bn

instance removeTrailingZerosI :: RemoveTrailingZeros r r' => RemoveTrailingZeros (Bc I r) (Bc I r')

instance removeTrailingZerosO ::
  ( AllZerosToNull r n
  , RemoveTrailingZeros r r'
  , BinEq n Bn tf
  , Gate tf Bn (Bc O r') x
  ) =>
  RemoveTrailingZeros (Bc O r) x

infixr 5 type PtrListCons as +:

-- | Like `Alt` for a list of pointers that also serves as an assertion that the positive result, if one exists, is a single pointer.
class PtrListKeepSingleton (ptrListA :: PtrList) (ptrListB :: PtrList) (ptrListC :: PtrList) | ptrListA ptrListB -> ptrListC

instance ptrListKeepSingletonNil :: PtrListKeepSingleton PtrListNil PtrListNil PtrListNil

instance ptrListKeepSingletonL :: PtrListKeepSingleton (PtrListCons a PtrListNil) PtrListNil (PtrListCons a PtrListNil)

instance ptrListKeepSingletonR :: PtrListKeepSingleton PtrListNil (PtrListCons a PtrListNil) (PtrListCons a PtrListNil)

-- | Alias for 0 in binary.
type D0
  = (Bc O Bn)

-- | Alias for 1 in binary.
type D1
  = (Bc I Bn)

-- | Alias for 2 in binary.
type D2
  = (Bc O (Bc I Bn))

-- | Alias for 3 in binary.
type D3
  = (Bc I (Bc I Bn))

-- | Alias for 4 in binary.
type D4
  = (Bc O (Bc O (Bc I Bn)))

-- | Alias for 5 in binary.
type D5
  = (Bc I (Bc O (Bc I Bn)))

-- | Alias for 6 in binary.
type D6
  = (Bc O (Bc I (Bc I Bn)))

-- | Alias for 7 in binary.
type D7
  = (Bc I (Bc I (Bc I Bn)))

-- | Alias for 8 in binary.
type D8
  = (Bc O (Bc O (Bc O (Bc I Bn))))

-- | Alias for 9 in binary.
type D9
  = (Bc I (Bc O (Bc O (Bc I Bn))))

-- | Alias for 10 in binary.
type D10
  = (Bc O (Bc I (Bc O (Bc I Bn))))

-- | Internal helper class to cast a binary value to an int.
class BinToInt (i :: Bits) where
  -- | Not for public use - used internally in public functions toInt' and toInt.
  toInt'' :: Int -> Proxy i -> Int

instance toIntBn :: BinToInt Bn where
  toInt'' _ _ = 0

instance toIntBcO :: BinToInt r => BinToInt (Bc O r) where
  toInt'' x _ = toInt'' (x `shl` 1) (Proxy :: _ r)

instance toIntBcI :: BinToInt r => BinToInt (Bc I r) where
  toInt'' x _ = x + toInt'' (x `shl` 1) (Proxy :: _ r)
