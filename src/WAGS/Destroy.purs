module WAGS.Destroy where

import Prelude

import Data.Functor (voidRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.AudioUnit (class TypeToSym)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC, NodeList)
import WAGS.Interpret (class AudioInterpret, destroyUnit)

idestroy
  :: forall proxy ptr audio engine proof res i o
   . AudioInterpret audio engine
  => Destroy ptr i o
  => proxy ptr
  -> IxWAG audio engine proof res i o Unit
idestroy ptr = IxWAG (destroy <<< voidRight ptr)

-- | Destroy node `ptr` in graph `i`, resulting in graph `o`. Note that, to destroy a node, it must have no outgoing or incoming edges. This is achieved via use of `disconnect`. Failure to disconnect nodes before destroying will result in a compile-time error during graph validation.
class Destroy (ptr :: Symbol) (i :: Graph) (o :: Graph) | ptr i -> o where
  destroy
    :: forall proxy audio engine proof res
     . AudioInterpret audio engine
    => WAG audio engine proof res i (proxy ptr)
    -> WAG audio engine proof res o Unit

instance destroyer ::
  ( IsSymbol ptr
  , TypeToSym node nodeSym
  , IsSymbol nodeSym
  , R.Cons ptr (node /\ {}) grapho graphi
  , RowToList graphi nodeListI
  , PointerNotPresentInAnyEdgeList ptr nodeListI
  ) =>
  Destroy ptr graphi grapho where
  destroy w =
    unsafeWAG
      $
        { context:
            i
              { instructions = i.instructions <> [ destroyUnit ptrI (reflectSymbol (Proxy :: _ nodeSym)) ]
              }
        , value: unit
        }
    where
    { context: i, value: ptrI' } = unsafeUnWAG w

    ptrI = reflectSymbol ptrI'

-- | Internal helper class used for destroing audio nodes.
class PointerNotPresentInAnyEdgeList (ptr :: Symbol) (i :: NodeList)

instance pointerNotPresentInAnyEdgeListNil :: PointerNotPresentInAnyEdgeList a RL.Nil

instance pointerNotPresentInAnyEdgeListCons :: (R.Lacks a l, PointerNotPresentInAnyEdgeList a tail) => PointerNotPresentInAnyEdgeList a (RL.Cons h (NodeC n { | l }) tail)
