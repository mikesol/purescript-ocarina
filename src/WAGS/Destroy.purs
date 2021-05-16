module WAGS.Destroy where

import Prelude
import Control.Monad.State (modify_)
import Data.Map as M
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Interpret (class AudioInterpret, destroyUnit)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC, NodeList)

-- | Destroy node `ptr` in graph `i`, resulting in graph `o`. Note that, to destroy a node, it must have no outgoing or incoming edges. This is achieved via use of `disconnect`. Failure to disconnect nodes before destroying will result in a compile-time error during graph validation.
class Destroy (ptr :: Symbol) (i :: Graph) (o :: Graph) | ptr i -> o where
  destroy :: forall proxy env audio engine proof m res. Monad m => AudioInterpret audio engine => proxy ptr -> FrameT env audio engine proof m res { | i } { | o } Unit

instance destroyer ::
  ( IsSymbol ptr
  , R.Cons ptr ignore0 grapho graphi
  , RowToList graphi nodeListI
  , PointerNotPresentInAnyEdgeList ptr nodeListI
  ) =>
  Destroy ptr graphi grapho where
  destroy ptrI' =
    unsafeFrame
      $ do
          modify_
            ( \i ->
                i
                  { internalNodes = M.delete ptrI (i.internalNodes)
                  , internalEdges = M.delete ptrI (i.internalEdges)
                  , instructions = i.instructions <> [ destroyUnit ptrI ]
                  }
            )
    where
    ptrI = reflectSymbol ptrI'

-- | Internal helper class used for destroing audio nodes.
class PointerNotPresentInAnyEdgeList (ptr :: Symbol) (i :: NodeList)

instance pointerNotPresentInAnyEdgeListNil :: PointerNotPresentInAnyEdgeList a RL.Nil

instance pointerNotPresentInAnyEdgeListCons :: (R.Lacks a l, PointerNotPresentInAnyEdgeList a tail) => PointerNotPresentInAnyEdgeList a (RL.Cons h (NodeC n { | l }) tail)
