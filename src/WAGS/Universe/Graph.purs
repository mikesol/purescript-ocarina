module WAGS.Universe.Graph where

import WAGS.Universe.Node (Node, NodeList, NodeListCons, NodeListNil)

-- | A graph of audio nodes.
data Graph

-- | The sole constructor of a graph. A graph is just a non-empty node list.
foreign import data GraphC :: Node -> NodeList -> Graph

foreign import data InitialGraph :: Graph

-- | Converts a graph to a node list and back again.
class GraphToNodeList (graph :: Graph) (nodeList :: NodeList) | graph -> nodeList, nodeList -> graph

instance graphToNodeList :: GraphToNodeList (GraphC node nodeList) (NodeListCons node nodeList)

instance graphToNodeListIG :: GraphToNodeList InitialGraph NodeListNil

