module WAGS.Tumult.Destroy where


import Data.Set (insert)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import WAGS.Tumult.Control.Types (WAG(..))
import WAGS.Tumult.Graph.AudioUnit (class TypeToSym)
import WAGS.Tumult.Graph.Graph (Graph)
import WAGS.Tumult.Graph.Node (NodeC, NodeList)
import WAGS.Tumult.Instructions as I

-- | Destroy node `ptr` in graph `i`, resulting in graph `o`. Note that, to destroy a node, it must have no outgoing or incoming edges. This is achieved via use of `disconnect`. Failure to disconnect nodes before destroying will result in a compile-time error during graph validation.
class Destroy (ptr :: Symbol) (i :: Graph) (o :: Graph) | ptr i -> o where
  destroy
    :: forall proxy
     . proxy ptr
    -> WAG i
    -> WAG o

instance destroyer ::
  ( IsSymbol ptr
  , TypeToSym node nodeSym
  , IsSymbol nodeSym
  , R.Cons ptr (node /\ {}) grapho graphi
  , RowToList graphi nodeListI
  , PointerNotPresentInAnyEdgeList ptr nodeListI
  ) =>
  Destroy ptr graphi grapho where
  destroy ptrI' w = WAG
    { instructions:
        insert
          ( I.iDestroyUnit { id, unit: reflectSymbol (Proxy :: _ nodeSym) }
          )
          instructions
    }
    where
    WAG { instructions } = w

    id = reflectSymbol ptrI'

-- | Internal helper class used for destroing audio nodes.
class PointerNotPresentInAnyEdgeList (ptr :: Symbol) (i :: NodeList)

instance pointerNotPresentInAnyEdgeListNil ::
  PointerNotPresentInAnyEdgeList a RL.Nil

instance pointerNotPresentInAnyEdgeListCons ::
  ( R.Lacks a l
  , PointerNotPresentInAnyEdgeList a tail
  ) =>
  PointerNotPresentInAnyEdgeList a (RL.Cons h (NodeC n { | l }) tail)
