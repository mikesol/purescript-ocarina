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

class MovePointer''' (from :: Nat) (to :: Nat) (i :: PtrList) (o :: PtrList) | from to i -> o

class InsertIn (fptr :: Ptr) (newTo :: Nat) (i' :: PtrList) (o :: PtrList) | fptr newTo i' -> o

instance insertInNil :: InsertIn fptr Z PtrListNil (PtrListCons fptr PtrListNil)

instance insertInZ :: InsertIn fptr Z (PtrListCons a b) (PtrListCons fptr (PtrListCons a b))

instance insertInSuc :: InsertIn fptr x b o => InsertIn fptr (Succ x) (PtrListCons a b) (PtrListCons a o)

class RemoveAt (from :: Nat) (i :: PtrList) (fptr :: Ptr) (i' :: PtrList) | from i -> fptr i'

instance removeAtZ :: RemoveAt Z (PtrListCons a b) a b

instance removeAtSucc :: RemoveAt x b fptr b' => RemoveAt (Succ x) (PtrListCons a b) fptr (PtrListCons a b')

instance movePointer''' ::
  ( PtrListLen i li
  , LtEq to li
  , RemoveAt from i fptr i'
  , InsertIn fptr to i' o
  ) =>
  MovePointer''' from to i o

class GetPtrIndex (found :: Type) (here :: Nat) (ptr :: Ptr) (i :: PtrList) (idx :: Nat) | found here ptr i -> idx

instance getPtrIdxTrue :: GetPtrIndex True here ptr i here

instance getPtrIdxFalse ::
  ( BinEq ptr a tf
  , Gate tf here (Succ here) idx
  , GetPtrIndex tf idx ptr b o
  ) =>
  GetPtrIndex False here ptr (PtrListCons a b) o

class MovePointer'' (from :: Type) (to :: Nat) (i :: PtrList) (o :: PtrList) | from to i -> o

instance movePointerARef ::
  ( GetPtrIndex False Z ptr i idx
  , MovePointer''' idx to i o
  ) =>
  MovePointer'' (AudioUnitRef ptr) to i o

instance movePointerZ :: (MovePointer''' Z to i o) => MovePointer'' (Proxy Z) to i o

instance movePointerSucc :: (MovePointer''' (Succ x) to i o) => MovePointer'' (Proxy (Succ x)) to i o

class MovePointer' (from :: Type) (to :: Nat) (i :: EdgeProfile) (o :: EdgeProfile) | from to i -> o

instance movePointer' :: MovePointer'' from to (PtrListCons a b) (PtrListCons x y) => MovePointer' from to (ManyEdges a b) (ManyEdges x y)

class MovePointer (at :: Ptr) (from :: Type) (to :: Nat) (i :: Node) (o :: Node) | at from to i -> o

instance movePointerGain :: MovePointer' from to (ManyEdges e l) oe => MovePointer at from to (NodeC (TGain at) (ManyEdges e l)) (NodeC (TGain at) oe)
else instance movePointerSpeaker :: MovePointer' from to (ManyEdges e l) oe => MovePointer at from to (NodeC (TSpeaker at) (ManyEdges e l)) (NodeC (TSpeaker at) oe)
else instance movePointerMiss :: MovePointer at from to i i

class MovePointers (at :: Ptr) (from :: Type) (to :: Nat) (i :: NodeList) (o :: NodeList) | at from to i -> o

instance movePointersNil :: MovePointers at a b NodeListNil NodeListNil

instance movePointersCons :: (MovePointer at a b head headRes, MovePointers at a b tail tailRes) => MovePointers at a b (NodeListCons head tail) (NodeListCons headRes tailRes)

class Move (at :: Ptr) (from :: Type) (to :: Nat) (i :: Universe) (o :: Universe) | at from to i -> o where
  move :: forall env audio engine proof m. Monad m => AudioUnitRef at -> from -> Proxy to -> FrameT env audio engine proof m i o Unit

instance moveAref ::
  ( GraphToNodeList graphi nodeListI
  , MovePointers at from to nodeListI nodeListO
  , GraphToNodeList grapho nodeListO
  ) =>
  Move at from to (UniverseC ptr graphi changeBit skolems) (UniverseC ptr grapho changeBit skolems) where
  move _ _ _ = unsafeFrame (pure unit)
