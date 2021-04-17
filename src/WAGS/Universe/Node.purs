module WAGS.Universe.Node where

import WAGS.Universe.AudioUnit (AudioUnit)
import WAGS.Universe.EdgeProfile (EdgeProfile)

-- | A single audio node in the graph.
data Node

-- | The sole constructor for an audio node. It is comprised of two distinct parts:
-- | - `AudioUnit` - the name of the node (ie a highpass filter, a sine-wave oscillator, etc) along with its pointer
-- | - `EdgeProfile` - incoming edges into the node
foreign import data NodeC :: AudioUnit -> EdgeProfile -> Node

-- | A list of audio nodes.
data NodeList

-- | Cons for the audio node list.
foreign import data NodeListCons :: Node -> NodeList -> NodeList

-- | Nil for the audio node list.
foreign import data NodeListNil :: NodeList

infixr 5 type NodeListCons as /:

infixr 5 type NodeC as /->

-- | This class acts as Alt for a `NodeList` with a supplemental assertion that the chosen list
-- | must contain at most one node.
class NodeListKeepSingleton (nodeListA :: NodeList) (nodeListB :: NodeList) (nodeListC :: NodeList) | nodeListA nodeListB -> nodeListC

instance nodeListKeepSingletonNil :: NodeListKeepSingleton NodeListNil NodeListNil NodeListNil

instance nodeListKeepSingletonL :: NodeListKeepSingleton (NodeListCons a NodeListNil) NodeListNil (NodeListCons a NodeListNil)

instance nodeListKeepSingletonR :: NodeListKeepSingleton NodeListNil (NodeListCons a NodeListNil) (NodeListCons a NodeListNil)

-- | Class to get the audio unit from a node.
class GetAudioUnit (node :: Node) (au :: AudioUnit) | node -> au

instance getAudioUnitNodeC :: GetAudioUnit (NodeC au ep) au
