module WAGS.Destroy where

import Prelude
import Data.Functor (voidRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC, NodeList)
import WAGS.Interpret (class AudioInterpret, destroyUnit)

idestroy ::
  forall proxy ptr assets audio engine proof res i o.
  AudioInterpret audio engine =>
  Destroy ptr i o =>
  proxy ptr ->
  IxWAG assets audio engine proof res { | i } { | o } Unit
idestroy ptr = IxWAG (destroy <<< voidRight ptr)

-- | Destroy node `ptr` in graph `i`, resulting in graph `o`. Note that, to destroy a node, it must have no outgoing or incoming edges. This is achieved via use of `disconnect`. Failure to disconnect nodes before destroying will result in a compile-time error during graph validation.
class Destroy (ptr :: Symbol) (i :: Graph) (o :: Graph) | ptr i -> o where
  destroy ::
    forall proxy assets audio engine proof res.
    AudioInterpret audio engine =>
    WAG assets audio engine proof res { | i } (proxy ptr) ->
    WAG assets audio engine proof res { | o } Unit

instance destroyer ::
  ( IsSymbol ptr
  , R.Cons ptr (node /\ {}) grapho graphi
  , RowToList graphi nodeListI
  , PointerNotPresentInAnyEdgeList ptr nodeListI
  ) =>
  Destroy ptr graphi grapho where
  destroy w =
    unsafeWAG
      $ { context:
            i
              { instructions = i.instructions <> [ destroyUnit ptrI ]
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
