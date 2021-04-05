module WAGS.Universe.Graph where

import WAGS.Universe.Node (Node, NodeList, NodeListCons, NodeListNil)

data Graph

-- non empty
foreign import data GraphC :: Node -> NodeList -> Graph

foreign import data InitialGraph :: Graph

class GraphToNodeList (graph :: Graph) (nodeList :: NodeList) | graph -> nodeList, nodeList -> graph

instance graphToNodeList :: GraphToNodeList (GraphC node nodeList) (NodeListCons node nodeList)

instance graphToNodeListIG :: GraphToNodeList InitialGraph NodeListNil

