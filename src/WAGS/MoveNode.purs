module WAGS.MoveNode where

import Prelude

import Type.Data.Peano (Nat, Succ, Z)
import Type.Proxy (Proxy)
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Universe.Graph (class GraphToNodeList, Graph)
import WAGS.Universe.Node (Node, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Util (class LtEq)

-- | Move a node in a graph.
-- |
-- | `from` - the index of the node we are moving
-- | `to` - the new position in the list
-- | `i` - the input universe
-- | `o` - the output universe
class MoveNode (from :: Nat) (to :: Nat) (i :: Graph) (o :: Graph) | from to i -> o where
  moveNode :: forall env audio engine proof m currentIdx changeBit skolems. Monad m => Proxy from -> Proxy to -> FrameT env audio engine proof m (UniverseC currentIdx i changeBit skolems) (UniverseC currentIdx o changeBit skolems) Unit

instance moveNodeAll ::
  ( GraphToNodeList graphi nodeListI
  , MoveNodeInternal from to nodeListI nodeListO
  , GraphToNodeList grapho nodeListO
  ) =>
  MoveNode from to graphi grapho where
  moveNode _ _ = unsafeFrame (pure unit)

-- | Get the length of node list `i` as natural number `n`/
class NodeListLen (i :: NodeList) (n :: Nat) | i -> n

instance nodeListLenZ :: NodeListLen NodeListNil Z

instance nodeListLenSucc :: NodeListLen b x => NodeListLen (NodeListCons a b) (Succ x)
instance moveNode''' ::
  ( NodeListLen i li
  , LtEq to li
  , RemoveAtNode from i fnode i'
  , InsertInNode fnode to i' o
  ) =>
  MoveNodeInternal from to i o

-- | Move a poitner from position `from` to position `to` in list `i` and return it as `o`.
class MoveNodeInternal (from :: Nat) (to :: Nat) (i :: NodeList) (o :: NodeList) | from to i -> o

-- | Insert a poitner `fnode` at `newTo` in list `i'` and return it as `o`.
class InsertInNode (fnode :: Node) (newTo :: Nat) (i' :: NodeList) (o :: NodeList) | fnode newTo i' -> o

instance insertInNil :: InsertInNode fnode Z NodeListNil (NodeListCons fnode NodeListNil)

instance insertInZ :: InsertInNode fnode Z (NodeListCons a b) (NodeListCons fnode (NodeListCons a b))

instance insertInSuc :: InsertInNode fnode x b o => InsertInNode fnode (Succ x) (NodeListCons a b) (NodeListCons a o)

-- | Remove whatever is at position `from` in list `i`, returning the removed `fnode` as  well as the new pointer list `i'`.
class RemoveAtNode (from :: Nat) (i :: NodeList) (fnode :: Node) (i' :: NodeList) | from i -> fnode i'

instance removeAtZ :: RemoveAtNode Z (NodeListCons a b) a b

instance removeAtSucc :: RemoveAtNode x b fnode b' => RemoveAtNode (Succ x) (NodeListCons a b) fnode (NodeListCons a b')

