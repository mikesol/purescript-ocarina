module WAGS.Universe.EdgeProfile where

import WAGS.Universe.Bin (Ptr, PtrList)

data EdgeProfile

-- non empty
foreign import data ManyEdges :: Ptr -> PtrList -> EdgeProfile

foreign import data SingleEdge :: Ptr -> EdgeProfile

foreign import data NoEdge :: EdgeProfile

