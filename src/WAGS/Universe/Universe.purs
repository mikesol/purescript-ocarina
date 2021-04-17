module WAGS.Universe.Universe where

import Type.Data.Peano (Nat)
import WAGS.Universe.Bin (Ptr)
import WAGS.Universe.Graph (Graph)
import WAGS.Universe.Skolems (SkolemList)


-- | The `Universe` is the top-level indexed construct for a `Frame`.
data Universe

-- | The sole constructor for a `Universe` is `UniverseC`. It takes the following parameters:
-- |
-- | - `currentIdx` - the current auto-incrementing index for pointers into the graph
-- | - `graph` - the audio graph
-- | - `changeBit` - how many times a given graph's control rate values have changed
-- | - `skolems` - used internally to hold skolem variables in the graph, which is necessary for recursive and duplicated audio units during the creation phase
foreign import data UniverseC :: Ptr -> Graph -> Nat -> SkolemList -> Universe

-- | Get the graph from a universe
class GetGraph (u :: Universe) (g :: Graph) | u -> g

instance getGraphUniverseC :: GetGraph (UniverseC currentIdx graph changeBit skolems) graph

