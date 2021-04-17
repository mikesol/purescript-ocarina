module WAGS.Universe.EdgeProfile where

import Prelude

import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import WAGS.Universe.AudioUnit (AudioUnitRef(..))
import WAGS.Universe.Bin (Ptr, PtrList, PtrListCons, PtrListNil)

data EdgeProfile

-- non empty
foreign import data ManyEdges :: Ptr -> PtrList -> EdgeProfile

foreign import data SingleEdge :: Ptr -> EdgeProfile

foreign import data NoEdge :: EdgeProfile

-- | An array of pointers that represents incoming edges to a node.
newtype PtrArr :: forall k. k -> Type
newtype PtrArr a
  = PtrArr (Array Int)

-- | Coerce an `AudioUnitRef` or a tuple of `AudioUnitRef`-s to an `EdgeProfile`.
class AsEdgeProfile a (b :: EdgeProfile) | a -> b where
  getPointers :: a -> PtrArr b
  asEdgeProfile :: a -> Proxy b

instance asEdgeProfileAR :: AsEdgeProfile (AudioUnitRef ptr) (SingleEdge ptr) where
  getPointers (AudioUnitRef i) = PtrArr [ i ]
  asEdgeProfile _ = Proxy
instance asEdgeProfileTupl :: EdgeListable x y => AsEdgeProfile (Tuple (AudioUnitRef ptr) x) (ManyEdges ptr y) where
  getPointers (Tuple (AudioUnitRef i) el) = let PtrArr o = getPointers' el in PtrArr ([ i ] <> o)
  asEdgeProfile _ = Proxy

-- | Gets incoming pointers as a pointer list.
class EdgeListable a (b :: PtrList) | a -> b where
  getPointers' :: a -> PtrArr b

instance edgeListableUnit :: EdgeListable Unit PtrListNil where
  getPointers' _ = PtrArr []

instance edgeListableTuple :: EdgeListable x y => EdgeListable (Tuple (AudioUnitRef ptr) x) (PtrListCons ptr y) where
  getPointers' (Tuple (AudioUnitRef i) x) = let PtrArr o = getPointers' x in PtrArr ([ i ] <> o)

