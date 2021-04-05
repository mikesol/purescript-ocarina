module WAGS.Universe where

import WAGS.Bin (Ptr, PtrList)

infixr 5 type NodeListCons as /:

infixr 5 type NodeC as /->

data AudioUnitList

foreign import data AudioUnitCons :: AudioUnit -> AudioUnitList -> AudioUnitList

foreign import data AudioUnitNil :: AudioUnitList

data SkolemPair

foreign import data SkolemPairC :: Type -> Ptr -> SkolemPair

data SkolemList

foreign import data SkolemListCons :: SkolemPair -> SkolemList -> SkolemList

foreign import data SkolemListNil :: SkolemList

data EdgeProfile

-- non empty
foreign import data ManyEdges :: Ptr -> PtrList -> EdgeProfile

foreign import data SingleEdge :: Ptr -> EdgeProfile

foreign import data NoEdge :: EdgeProfile

data AudioUnit

foreign import data TSinOsc :: Ptr -> AudioUnit

foreign import data THighpass :: Ptr -> AudioUnit

foreign import data TGain :: Ptr -> AudioUnit

foreign import data TSpeaker :: Ptr -> AudioUnit

data Node

foreign import data NodeC :: AudioUnit -> EdgeProfile -> Node

data NodeList

foreign import data NodeListCons :: Node -> NodeList -> NodeList

foreign import data NodeListNil :: NodeList

data Graph

-- non empty
foreign import data GraphC :: Node -> NodeList -> Graph

foreign import data InitialGraph :: Graph

data Universe

-- currentIdx graph skolems accumulator
foreign import data UniverseC :: Ptr -> Graph -> SkolemList -> Universe
