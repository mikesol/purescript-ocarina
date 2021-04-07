module WAGS.Validation where

import Data.Typelevel.Bool (False, True)
import WAGS.Universe.AudioUnit (class AudioUnitEq, class GetPointer, AudioUnit, AudioUnitCons, AudioUnitList, AudioUnitNil, TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Universe.Bin (class BinEq, Ptr, PtrList, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, Graph)
import WAGS.Universe.Node (class GetAudioUnit, class NodeListKeepSingleton, Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (class GetGraph, Universe)
import WAGS.Util (class Gate)


class LookupNL (accumulator :: NodeList) (ptr :: Ptr) (graph :: NodeList) (node :: NodeList) | accumulator ptr graph -> node

instance lookupNLNil :: LookupNL accumulator ptr NodeListNil accumulator

instance lookupNLNilCons ::
  ( GetAudioUnit head headAU
  , GetPointer headAU maybePtr
  , BinEq maybePtr ptr tf
  , Gate tf (NodeListCons head NodeListNil) NodeListNil toComp
  , NodeListKeepSingleton toComp accumulator acc
  , LookupNL acc ptr tail o
  ) =>
  LookupNL accumulator ptr (NodeListCons head tail) o

---------------------------
------------ NoNodesAreDuplicated
class NodeNotInNodeList (node :: Node) (nodeList :: NodeList)

instance nodeNotInNodeListNil :: NodeNotInNodeList node NodeListNil

instance nodeNotInNodeListCons ::
  ( GetAudioUnit node nodeAu
  , GetAudioUnit head headAu
  , AudioUnitEq nodeAu headAu False
  , NodeNotInNodeList node tail
  ) =>
  NodeNotInNodeList node (NodeListCons head tail)

class NoNodesAreDuplicatedInNodeList (nodeList :: NodeList)

instance noNodesAreDuplicatedInNodeListNil :: NoNodesAreDuplicatedInNodeList NodeListNil

instance noNodesAreDuplicatedInNodeListCons ::
  ( NodeNotInNodeList head tail
  , NoNodesAreDuplicatedInNodeList tail
  ) =>
  NoNodesAreDuplicatedInNodeList (NodeListCons head tail)

class NoNodesAreDuplicated (graph :: Graph)

instance noNodesAreDuplicated ::
  ( GraphToNodeList graph nodeList
  , NoNodesAreDuplicatedInNodeList nodeList
  ) =>
  NoNodesAreDuplicated graph

---------------------------
------------ AllEdgesPointToNodes
class PtrInPtrList (foundPtr :: Type) (ptr :: Ptr) (nodeList :: PtrList) (output :: Type) | foundPtr ptr nodeList -> output

instance ptrInPtrListTrue :: PtrInPtrList True a b True

instance ptrInPtrListFalseNil :: PtrInPtrList False a PtrListNil False

instance ptrInPtrListFalseCons ::
  ( BinEq ptr head foundNode
  , PtrInPtrList foundNode ptr tail o
  ) =>
  PtrInPtrList False ptr (PtrListCons head tail) o

class AudioUnitInAudioUnitList (foundNode :: Type) (audioUnit :: AudioUnit) (audioUnitList :: AudioUnitList) (output :: Type) | foundNode audioUnit audioUnitList -> output

instance audioUnitInAudioUnitListTrue :: AudioUnitInAudioUnitList True a b True

instance audioUnitInAudioUnitListFalseNil :: AudioUnitInAudioUnitList False a AudioUnitNil False

instance audioUnitInAudioUnitListFalseCons ::
  ( AudioUnitEq au head foundNode
  , AudioUnitInAudioUnitList foundNode au tail o
  ) =>
  AudioUnitInAudioUnitList False au (AudioUnitCons head tail) o

class AllPtrsInNodeList (needles :: PtrList) (haystack :: NodeList)

instance allPtrsInNodeList :: AllPtrsInNodeList PtrListNil haystack

instance allPtrsInNodeListCons ::
  ( LookupNL NodeListNil head haystack (NodeListCons x NodeListNil)
  , AllPtrsInNodeList tail haystack
  ) =>
  AllPtrsInNodeList (PtrListCons head tail) haystack

class GetEdgesAsPtrList (node :: Node) (ptrList :: PtrList) | node -> ptrList

instance getEdgesAsPtrListNoEdge :: GetEdgesAsPtrList (NodeC x NoEdge) PtrListNil

instance getEdgesAsPtrListSingleEdge :: GetEdgesAsPtrList (NodeC x (SingleEdge e)) (PtrListCons e PtrListNil)

instance getEdgesAsPtrListManyEdges :: GetEdgesAsPtrList (NodeC x (ManyEdges e l)) (PtrListCons e l)

class AllEdgesInNodeList (needles :: NodeList) (haystack :: NodeList)

instance allEdgesInNodeListNil :: AllEdgesInNodeList NodeListNil haystack

instance allEdgesInNodeListCons ::
  ( GetEdgesAsPtrList head ptrList
  , AllPtrsInNodeList ptrList haystack
  , AllEdgesInNodeList tail haystack
  ) =>
  AllEdgesInNodeList (NodeListCons head tail) haystack

class AllEdgesPointToNodes (graph :: Graph)

instance allEdgesPointToNodes :: (GraphToNodeList graph nodeList, AllEdgesInNodeList nodeList nodeList) => AllEdgesPointToNodes graph

----------------------- NoParallelEdges
------- NoParallelEdges
class PtrNotInPtrList (ptr :: Ptr) (ptrList :: PtrList)

instance ptrNotInPtrListNil :: PtrNotInPtrList ptr PtrListNil

instance ptrNotInPtrListCons ::
  ( BinEq ptr head False
  , PtrNotInPtrList ptr tail
  ) =>
  PtrNotInPtrList ptr (PtrListCons head tail)

class NoPtrsAreDuplicatedInPtrList (ptrList :: PtrList)

instance noPtrsAreDuplicatedInPtrListNil :: NoPtrsAreDuplicatedInPtrList PtrListNil

instance noPtrsAreDuplicatedInPtrListCons ::
  ( PtrNotInPtrList head tail
  , NoPtrsAreDuplicatedInPtrList tail
  ) =>
  NoPtrsAreDuplicatedInPtrList (PtrListCons head tail)

class NoParallelEdgesNL (nodeList :: NodeList)

instance noParallelEdgesNLNil :: NoParallelEdgesNL NodeListNil

instance noParallelEdgesNLConsNoEdge :: (NoParallelEdgesNL tail) => NoParallelEdgesNL (NodeListCons (NodeC n NoEdge) tail)

instance noParallelEdgesNLConsSingleEdge :: (NoParallelEdgesNL tail) => NoParallelEdgesNL (NodeListCons (NodeC n (SingleEdge e)) tail)

instance noParallelEdgesNLConsManyEdges ::
  ( NoPtrsAreDuplicatedInPtrList (PtrListCons e l)
  , NoParallelEdgesNL tail
  ) =>
  NoParallelEdgesNL (NodeListCons (NodeC n (ManyEdges e l)) tail)

class NoParallelEdges (graph :: Graph)

instance noParallelEdges ::
  ( GraphToNodeList graph nodeList
  , NoParallelEdgesNL nodeList
  ) =>
  NoParallelEdges graph

------------- UniqueTerminus
-------- UniqueTerminus
class BottomLevelNodesNL (accumulator :: NodeList) (toTraverse :: NodeList) (output :: NodeList) | accumulator toTraverse -> output

instance bottomLevelNodesNLNil :: BottomLevelNodesNL accumulator NodeListNil accumulator

instance bottomLevelNodesNLConsNoEdge :: BottomLevelNodesNL (NodeListCons (NodeC i NoEdge) accumulator) tail o => BottomLevelNodesNL accumulator (NodeListCons (NodeC i NoEdge) tail) o

instance bottomLevelNodesNLConsSingleEdge :: BottomLevelNodesNL accumulator tail o => BottomLevelNodesNL accumulator (NodeListCons (NodeC i (SingleEdge x)) tail) o

instance bottomLevelNodesNLConsManyEdges :: BottomLevelNodesNL accumulator tail o => BottomLevelNodesNL accumulator (NodeListCons (NodeC i (ManyEdges x l)) tail) o

class BottomLevelNodes (graph :: Graph) (nodeList :: NodeList) | graph -> nodeList

instance bottomLevelNodes ::
  ( GraphToNodeList graph nodeList
  , BottomLevelNodesNL NodeListNil nodeList bottomLevelNodes
  ) =>
  BottomLevelNodes graph bottomLevelNodes

class HasBottomLevelNodes (graph :: Graph)

instance hasBottomLevelNodes :: BottomLevelNodes graph (NodeListCons a b) => HasBottomLevelNodes graph

class AudioUnitInNodeList (foundNode :: Type) (audioUnit :: AudioUnit) (nodeList :: NodeList) (output :: Type) | foundNode audioUnit nodeList -> output

instance audioUnitInNodeListTrue :: AudioUnitInNodeList True a b True

instance audioUnitInNodeListFalseNil :: AudioUnitInNodeList False a NodeListNil False

instance audioUnitInNodeListFalseCons ::
  ( GetAudioUnit head headAu
  , AudioUnitEq au headAu foundNode
  , AudioUnitInNodeList foundNode au tail o
  ) =>
  AudioUnitInNodeList False au (NodeListCons head tail) o

class RemoveDuplicates (accumulator :: NodeList) (maybeWithDuplicates :: NodeList) (removed :: NodeList) | accumulator maybeWithDuplicates -> removed

instance removeDuplicatesNil :: RemoveDuplicates accumulator NodeListNil accumulator

instance removeDuplicatesCons ::
  ( GetAudioUnit head au
  , AudioUnitInNodeList False au accumulator tf
  , Gate tf accumulator (NodeListCons head accumulator) acc
  , RemoveDuplicates acc tail o
  ) =>
  RemoveDuplicates accumulator (NodeListCons head tail) o

class AssertSingleton (maybeSingleton :: NodeList) (singleton :: Node) | maybeSingleton -> singleton

instance getUniqueTerminusCons :: AssertSingleton (NodeListCons n NodeListNil) n

class NodeInNodeList (foundNode :: Type) (node :: Node) (nodeList :: NodeList) (output :: Type) | foundNode node nodeList -> output

instance nodeInNodeListTrue :: NodeInNodeList True a b True

instance nodeInNodeListFalseNil :: NodeInNodeList False a NodeListNil False

instance nodeInNodeListFalseCons ::
  ( GetAudioUnit head headAu
  , GetAudioUnit node nodeAu
  , AudioUnitEq nodeAu headAu foundNode
  , NodeInNodeList foundNode node tail o
  ) =>
  NodeInNodeList False node (NodeListCons head tail) o

class UnvisitedNodes (visited :: NodeList) (accumulator :: NodeList) (candidates :: NodeList) (unvisited :: NodeList) | visited accumulator candidates -> unvisited

instance unvisitedNodesNil :: UnvisitedNodes visited accumulator NodeListNil accumulator

instance unvisitedNodesCons ::
  ( NodeInNodeList False head visited tf
  , Gate tf accumulator (NodeListCons head accumulator) acc
  , UnvisitedNodes visited acc tail o
  ) =>
  UnvisitedNodes visited accumulator (NodeListCons head tail) o

class ToVisitSingle (accumulator :: NodeList) (graph :: NodeList) (findMeInAnEdge :: Node) (candidates :: NodeList) | accumulator graph findMeInAnEdge -> candidates

instance toVisitSingleNil :: ToVisitSingle accumulator NodeListNil findMeInAnEdge accumulator

instance toVisitSingleCons ::
  ( GetEdgesAsPtrList head edgeList
  , GetAudioUnit findMeInAnEdge au
  , GetPointer au ptr
  , PtrInPtrList False ptr edgeList tf
  , Gate tf (NodeListCons head accumulator) accumulator acc
  , ToVisitSingle acc tail findMeInAnEdge o
  ) =>
  ToVisitSingle accumulator (NodeListCons head tail) findMeInAnEdge o

class NodeListAppend (l :: NodeList) (r :: NodeList) (o :: NodeList) | l r -> o

instance nodeListAppendNilNil :: NodeListAppend NodeListNil NodeListNil NodeListNil

instance nodeListAppendNilL :: NodeListAppend NodeListNil (NodeListCons x y) (NodeListCons x y)

instance nodeListAppendNilR :: NodeListAppend (NodeListCons x y) NodeListNil (NodeListCons x y)

instance nodeListAppendCons :: (NodeListAppend b (NodeListCons c d) o) => NodeListAppend (NodeListCons a b) (NodeListCons c d) (NodeListCons a o)

class PtrListAppend (l :: PtrList) (r :: PtrList) (o :: PtrList) | l r -> o

instance ptrListAppendNilNil :: PtrListAppend PtrListNil PtrListNil PtrListNil

instance ptrListAppendNilL :: PtrListAppend PtrListNil (PtrListCons x y) (PtrListCons x y)

instance ptrListAppendNilR :: PtrListAppend (PtrListCons x y) PtrListNil (PtrListCons x y)

instance ptrListAppendCons :: (PtrListAppend b (PtrListCons c d) o) => PtrListAppend (PtrListCons a b) (PtrListCons c d) (PtrListCons a o)

class EdgeProfileChooseGreater (a :: EdgeProfile) (b :: EdgeProfile) (c :: EdgeProfile) | a b -> c

instance edgeProfileChooseGreater0 :: EdgeProfileChooseGreater NoEdge b b
else instance edgeProfileChooseGreater1 :: EdgeProfileChooseGreater a NoEdge a

class IsNodeListEmpty (nodeList :: NodeList) (tf :: Type) | nodeList -> tf

instance isNodeListEmptyNil :: IsNodeListEmpty NodeListNil True

instance isNodeListEmptyCons :: IsNodeListEmpty (NodeListCons a b) False

class ToVisit (candidatesAccumulator :: NodeList) (parentlessAccumulator :: NodeList) (graph :: NodeList) (findMeInAnEdge :: NodeList) (candidates :: NodeList) (parentless :: NodeList) | candidatesAccumulator parentlessAccumulator graph findMeInAnEdge -> candidates parentless

instance toVisitNil :: ToVisit candidatesAccumulator parentlessAccumulator graph NodeListNil candidatesAccumulator parentlessAccumulator

instance toVisitCons ::
  ( ToVisitSingle NodeListNil graph findMeInAnEdge o
  , IsNodeListEmpty o tf
  , Gate tf (NodeListCons findMeInAnEdge parentlessAccumulator) parentlessAccumulator pA
  , NodeListAppend candidatesAccumulator o cA
  , ToVisit cA pA graph tail ccA ppA
  ) =>
  ToVisit candidatesAccumulator parentlessAccumulator graph (NodeListCons findMeInAnEdge tail) ccA ppA

class TerminusLoop (graph :: NodeList) (visited :: NodeList) (visiting :: NodeList) (accumulator :: NodeList) (output :: NodeList) | graph visited visiting accumulator -> output

instance terminusLoopNil :: TerminusLoop graph visited NodeListNil accumulator accumulator

instance terminusLoopCons ::
  ( ToVisit NodeListNil NodeListNil graph (NodeListCons a b) candidatesDup parentless
  -- remove duplicates in case where we have many nodes pointing to one node
  -- in this case, it would be in candidates multiple times
  -- we remove duplicates from the parent only at the end, as we are not recursing over it
  , RemoveDuplicates NodeListNil candidatesDup candidates
  , UnvisitedNodes visited NodeListNil candidates unvisited
  , NodeListAppend unvisited visited newVisited
  , NodeListAppend accumulator parentless newAccumulator
  , TerminusLoop graph newVisited unvisited newAccumulator o
  ) =>
  TerminusLoop graph visited (NodeListCons a b) accumulator o

class UniqueTerminus (graph :: Graph) (node :: Node) | graph -> node

instance uniqueTerminus ::
  ( BottomLevelNodes graph bottomLevel
  , GraphToNodeList graph graphAsNodeList
  , TerminusLoop graphAsNodeList NodeListNil bottomLevel NodeListNil terminii
  -- we remove duplicates from the parent only after terminus loop, as we are not recursing over it in the loop
  , RemoveDuplicates NodeListNil terminii unduped
  , AssertSingleton unduped node
  ) =>
  UniqueTerminus graph node

class HasUniqueTerminus (graph :: Graph)

instance hasUniqueTerminus :: UniqueTerminus graph node => HasUniqueTerminus graph

-----------------------------------------
-------------- Need to check fully hydrated
-------------- Even though we have a unique terminus, we don't want a state where a gain is a bottom node
class AllNodesAreFullyHydratedNL (graph :: NodeList)

instance allNodesAreFullyHydratedNil :: AllNodesAreFullyHydratedNL NodeListNil

instance allNodesAreFullyHydratedConsTSinOsc :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TSinOsc a) NoEdge) tail)

instance allNodesAreFullyHydratedConsTHighpass :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (THighpass a) (SingleEdge e)) tail)

instance allNodesAreFullyHydratedConsTGainSE :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TGain a) (SingleEdge e)) tail)

instance allNodesAreFullyHydratedConsTGainME :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TGain a) (ManyEdges e l)) tail)

instance allNodesAreFullyHydratedConsTSpeakerSE :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TSpeaker a) (SingleEdge e)) tail)

instance allNodesAreFullyHydratedConsTSpeakerME :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TSpeaker a) (ManyEdges e l)) tail)

class AllNodesAreFullyHydrated (graph :: Graph)

instance allNodesAreFullyHydrated :: (GraphToNodeList graph nodeList, AllNodesAreFullyHydratedNL nodeList) => AllNodesAreFullyHydrated graph

class NodeIsOutputDevice (node :: Node)

instance nodeIsOutputDeviceTSpeaker :: NodeIsOutputDevice (NodeC (TSpeaker a) x)

class GraphIsRenderable (graph :: Graph)

instance graphIsRenderable ::
  ( NoNodesAreDuplicated graph
  , AllEdgesPointToNodes graph
  , NoParallelEdges graph
  , HasBottomLevelNodes graph
  , UniqueTerminus graph terminus
  , NodeIsOutputDevice terminus
  , AllNodesAreFullyHydrated graph
  ) =>
  GraphIsRenderable graph

class TerminalNode (u :: Universe) (ptr :: Ptr) | u -> ptr

instance terminalNode ::
  ( GetGraph i g
  , UniqueTerminus g t
  , GetAudioUnit t u
  , GetPointer u ptr
  ) =>
  TerminalNode i ptr

class TerminalIdentityEdge (u :: Universe) (prof :: EdgeProfile) | u -> prof

instance terminalIdentityEdge :: (TerminalNode i ptr) => TerminalIdentityEdge i (SingleEdge ptr)