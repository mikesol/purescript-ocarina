module WAGS.Move where

import Prelude
import Data.Typelevel.Bool (False, True)
import Type.Data.Peano (Nat, Succ, Z)
import Type.Proxy (Proxy)
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Universe.AudioUnit (AudioUnitRef, TGain, TSpeaker)
import WAGS.Universe.Bin (class BinEq, Ptr, PtrList, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges)
import WAGS.Universe.Graph (class GraphToNodeList)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (Universe, UniverseC)
import WAGS.Util (class Gate)

-- | Move an edge in an edge list for a given node.
-- | As an example, if we have a gain node with incoming edges `7,8,9`, this can be used to move `7`
-- | to the end of the list so that we have `8,9,7`.
-- | This is useful in conjunction with rebasing if two graphs are almost similar save their
-- | edge lists, which have a different order.
-- |
-- | `at` - a ptr to the node whose edges we will move
-- | `from` - the edge we are moving - either an index (`Nat`) or audio unit ref
-- | `to` - the new position in the list
-- | `i` - the input universe
-- | `o` - the output universe
class Move (at :: Ptr) (from :: Type) (to :: Nat) (i :: Universe) (o :: Universe) | at from to i -> o where
  move :: forall env audio engine proof m. Monad m => AudioUnitRef at -> from -> Proxy to -> FrameT env audio engine proof m i o Unit

instance moveAref ::
  ( GraphToNodeList graphi nodeListI
  , MovePointers at from to nodeListI nodeListO
  , GraphToNodeList grapho nodeListO
  ) =>
  Move at from to (UniverseC ptr graphi changeBit skolems) (UniverseC ptr grapho changeBit skolems) where
  move _ _ _ = unsafeFrame (pure unit)

-- | Get the length of pointer list `i` as natural number `n`/
class PtrListLen (i :: PtrList) (n :: Nat) | i -> n

instance ptrListLenZ :: PtrListLen PtrListNil Z

instance ptrListLenSucc :: PtrListLen b x => PtrListLen (PtrListCons a b) (Succ x)

-- | Is `a` less than `b`? True or False (`c`)
class LtTf (a :: Nat) (b :: Nat) (c :: Type) | a b -> c

instance ltTfZ :: LtTf Z Z False

instance ltTfZ' :: LtTf Z (Succ x) True

instance ltTfS :: LtTf x y tf => LtTf (Succ x) (Succ y) tf

-- | Assertion that `a` is less than `b`
class LtEq (a :: Nat) (b :: Nat)

instance ltEqZ :: LtEq Z Z

instance ltEqZ' :: LtEq Z (Succ x)

instance ltEqS :: LtEq x y => LtEq (Succ x) (Succ y)

instance movePointer''' ::
  ( PtrListLen i li
  , LtEq to li
  , RemoveAt from i fptr i'
  , InsertIn fptr to i' o
  ) =>
  MovePointer''' from to i o

-- | Move a poitner from position `from` to position `to` in list `i` and return it as `o`.
class MovePointer''' (from :: Nat) (to :: Nat) (i :: PtrList) (o :: PtrList) | from to i -> o

-- | Insert a poitner `fptr` at `newTo` in list `i'` and return it as `o`.
class InsertIn (fptr :: Ptr) (newTo :: Nat) (i' :: PtrList) (o :: PtrList) | fptr newTo i' -> o

instance insertInNil :: InsertIn fptr Z PtrListNil (PtrListCons fptr PtrListNil)

instance insertInZ :: InsertIn fptr Z (PtrListCons a b) (PtrListCons fptr (PtrListCons a b))

instance insertInSuc :: InsertIn fptr x b o => InsertIn fptr (Succ x) (PtrListCons a b) (PtrListCons a o)

-- | Remove whatever is at position `from` in list `i`, returning the removed `fptr` as  well as the new pointer list `i'`.
class RemoveAt (from :: Nat) (i :: PtrList) (fptr :: Ptr) (i' :: PtrList) | from i -> fptr i'

instance removeAtZ :: RemoveAt Z (PtrListCons a b) a b

instance removeAtSucc :: RemoveAt x b fptr b' => RemoveAt (Succ x) (PtrListCons a b) fptr (PtrListCons a b')

-- | Tail recursive function to get assert that a pointer is at an index
-- | - `found` - the accumulator of whether we've found the pointer or not
-- | - `here` - the current index in the recursive function, starts at Z
-- | - `ptr` - the pointer we are looking for
-- | - `i` - the pointer list
-- | - `idx` - the index at which the pointer is found if it is found
class GetPtrIndex (found :: Type) (here :: Nat) (ptr :: Ptr) (i :: PtrList) (idx :: Nat) | found here ptr i -> idx

instance getPtrIdxTrue :: GetPtrIndex True here ptr i here

instance getPtrIdxFalse ::
  ( BinEq ptr a tf
  , Gate tf here (Succ here) idx
  , GetPtrIndex tf idx ptr b o
  ) =>
  GetPtrIndex False here ptr (PtrListCons a b) o

-- | Move a poitner from `from`, which is either a position or a reference to a value at an index, to position `to` in list `i` and return it as `o`.
class MovePointer'' (from :: Type) (to :: Nat) (i :: PtrList) (o :: PtrList) | from to i -> o

instance movePointerARef ::
  ( GetPtrIndex False Z ptr i idx
  , MovePointer''' idx to i o
  ) =>
  MovePointer'' (AudioUnitRef ptr) to i o

instance movePointerZ :: (MovePointer''' Z to i o) => MovePointer'' (Proxy Z) to i o

instance movePointerSucc :: (MovePointer''' (Succ x) to i o) => MovePointer'' (Proxy (Succ x)) to i o

-- | Move `from`, which is either a position or a reference to a value at an index, to position `to` in edge profile `i` and return the edge profile as `o`.
class MovePointer' (from :: Type) (to :: Nat) (i :: EdgeProfile) (o :: EdgeProfile) | from to i -> o

instance movePointer' :: MovePointer'' from to (PtrListCons a b) (PtrListCons x y) => MovePointer' from to (ManyEdges a b) (ManyEdges x y)

class MovePointer (at :: Ptr) (from :: Type) (to :: Nat) (i :: Node) (o :: Node) | at from to i -> o

instance movePointerGain :: MovePointer' from to (ManyEdges e l) oe => MovePointer at from to (NodeC (TGain at) (ManyEdges e l)) (NodeC (TGain at) oe)
else instance movePointerSpeaker :: MovePointer' from to (ManyEdges e l) oe => MovePointer at from to (NodeC (TSpeaker at) (ManyEdges e l)) (NodeC (TSpeaker at) oe)
else instance movePointerMiss :: MovePointer at from to i i

-- | At pointer `at`, move `from`, which is either a position or a reference to a value at an index, to position `to` in edge profile `i` and return the edge profile as `o`.
class MovePointers (at :: Ptr) (from :: Type) (to :: Nat) (i :: NodeList) (o :: NodeList) | at from to i -> o

instance movePointersNil :: MovePointers at a b NodeListNil NodeListNil

instance movePointersCons :: (MovePointer at a b head headRes, MovePointers at a b tail tailRes) => MovePointers at a b (NodeListCons head tail) (NodeListCons headRes tailRes)
