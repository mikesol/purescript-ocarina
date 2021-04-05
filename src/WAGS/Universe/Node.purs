module WAGS.Universe.Node where

import WAGS.Universe.AudioUnit (AudioUnit)
import WAGS.Universe.EdgeProfile (EdgeProfile)

data Node

foreign import data NodeC :: AudioUnit -> EdgeProfile -> Node

data NodeList

foreign import data NodeListCons :: Node -> NodeList -> NodeList

foreign import data NodeListNil :: NodeList

infixr 5 type NodeListCons as /:

infixr 5 type NodeC as /->


class NodeListKeepSingleton (nodeListA :: NodeList) (nodeListB :: NodeList) (nodeListC :: NodeList) | nodeListA nodeListB -> nodeListC

instance nodeListKeepSingletonNil :: NodeListKeepSingleton NodeListNil NodeListNil NodeListNil

instance nodeListKeepSingletonL :: NodeListKeepSingleton (NodeListCons a NodeListNil) NodeListNil (NodeListCons a NodeListNil)

instance nodeListKeepSingletonR :: NodeListKeepSingleton NodeListNil (NodeListCons a NodeListNil) (NodeListCons a NodeListNil)

class GetAudioUnit (node :: Node) (au :: AudioUnit) | node -> au

instance getAudioUnitNodeC :: GetAudioUnit (NodeC au ep) au
