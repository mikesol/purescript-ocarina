module WAGS.Universe.Universe where

import WAGS.Universe.Bin (Ptr)
import WAGS.Universe.Graph (Graph)
import WAGS.Universe.Skolems (SkolemList)


data Universe

-- currentIdx graph skolems accumulator
foreign import data UniverseC :: Ptr -> Graph -> SkolemList -> Universe

class GetGraph (u :: Universe) (g :: Graph) | u -> g

instance getGraphUniverseC :: GetGraph (UniverseC ptr graph skolems) graph

