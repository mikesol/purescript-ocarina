-- | Validation algorithms for audio graphs.
-- |
-- | Validation includes making sure the graph is renderable by
-- | the web audio API and retrieving information about valid graphs,
-- | like their terminal node (ie a loudspeaker).
module WAGS.Validation where

import Data.Typelevel.Bool (False, True)
import WAGS.Universe.AudioUnit (class AudioUnitEq, class GetPointer, AudioUnit, AudioUnitCons, AudioUnitList, AudioUnitNil)
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinEq, Ptr, PtrList, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, Graph)
import WAGS.Universe.Node (class GetAudioUnit, class NodeListKeepSingleton, Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Util (class Gate)

-- | Asserts that an audio graph is renderable.
-- |
-- | Every time that `makeScene` is called, we assert that the graph is
-- | renderable. This means that it can be played by the WebAudio API
-- | without any runtime errors. Note that this assertion is a
-- | _compile time_ assertion and not a _runtime_ assertion. This means that,
-- | when your program is compiled, it is guaranteed to be renderable by
-- | the WebAudio API _assuming that_ it is within the memory constraints
-- | of a given browser and computer. Usually, these memory limits are not
-- | hit unless one has hundreds of active nodes.
class GraphIsRenderable (graph :: Graph)

instance graphIsRenderable ::
  ( NoNodesAreDuplicated graph
  , AllEdgesPointToNodes graph
  , NoParallelEdges graph
  , HasSourceNodes graph
  , UniqueTerminus graph terminus
  , NodeIsOutputDevice terminus
  , AllNodesAreSaturated graph
  ) =>
  GraphIsRenderable graph

-- | Retrieves the terminal node from an audio grpah. This is
-- | almost always a speaker or recording, but if the graph is
-- | at an intermediary stage of construction, this could be another
-- | top-level node, ie a gain node.
class TerminalNode (g :: Graph) (ptr :: Ptr) | g -> ptr

instance terminalNode ::
  ( UniqueTerminus g t
  , GetAudioUnit t u
  , GetPointer u ptr
  ) =>
  TerminalNode g ptr

-- | Retrieves the terminal node from an audio grpah as an
-- | edge profile - in this case, a single edge into the terminal node.
class TerminalIdentityEdge (u :: Graph) (prof :: EdgeProfile) | u -> prof

instance terminalIdentityEdge :: (TerminalNode i ptr) => TerminalIdentityEdge i (SingleEdge ptr)

-----------------------
-----------------------
-----------------------
-----------------------

-- | Tail recursive lookup of a node in a node list.
-- | - `accumulator`: the accumulator for tail recursion that should start at `NodeListNil`.
-- | - `ptr`: the pointer of the node we are trying retrieve.
-- | - `graph`: the list of nodes to iterate over.
-- | - `node`: a list of nodes that have this pointer.
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


-- | Assertion that `node` is not present in `nodeList`.
class NodeNotInNodeList (node :: Node) (nodeList :: NodeList)

instance nodeNotInNodeListNil :: NodeNotInNodeList node NodeListNil

instance nodeNotInNodeListCons ::
  ( GetAudioUnit node nodeAu
  , GetAudioUnit head headAu
  , AudioUnitEq nodeAu headAu False
  , NodeNotInNodeList node tail
  ) =>
  NodeNotInNodeList node (NodeListCons head tail)

-- | Assertion that there are no duplicate nodes in `nodeList`.
class NoNodesAreDuplicatedInNodeList (nodeList :: NodeList)

instance noNodesAreDuplicatedInNodeListNil :: NoNodesAreDuplicatedInNodeList NodeListNil

instance noNodesAreDuplicatedInNodeListCons ::
  ( NodeNotInNodeList head tail
  , NoNodesAreDuplicatedInNodeList tail
  ) =>
  NoNodesAreDuplicatedInNodeList (NodeListCons head tail)

-- | Assertion that there are no duplicate nodes in `graph`.
class NoNodesAreDuplicated (graph :: Graph)

instance noNodesAreDuplicated ::
  ( GraphToNodeList graph nodeList
  , NoNodesAreDuplicatedInNodeList nodeList
  ) =>
  NoNodesAreDuplicated graph

-- | Tail-recursive check that a pointer is in a pointer list.
-- | - `foundPtr`: an accumulator indicating whether we've found the ptr or not. Starts as False.
-- | - `ptr`: is this ptr in the pointer list?
-- | - `ptrList`: a list of pointers
-- | - `output`: `True` if the pointer is in the list, `False` otherwise.
class PtrInPtrList (foundPtr :: Type) (ptr :: Ptr) (ptrList :: PtrList) (output :: Type) | foundPtr ptr ptrList -> output

instance ptrInPtrListTrue :: PtrInPtrList True a b True

instance ptrInPtrListFalseNil :: PtrInPtrList False a PtrListNil False

instance ptrInPtrListFalseCons ::
  ( BinEq ptr head foundNode
  , PtrInPtrList foundNode ptr tail o
  ) =>
  PtrInPtrList False ptr (PtrListCons head tail) o

-- | Tail-recursive check that an audio unit is in an audio unit list.
-- | - `foundPtr`: an accumulator indicating whether we've found the audio unit or not. Starts as False.
-- | - `audioUnit`: is this audioUnit in the audio unit list?
-- | - `audioUnitList`: a list of audio units
-- | - `output`: `True` if the audio unit is in the list, `False` otherwise.
class AudioUnitInAudioUnitList (foundAU :: Type) (audioUnit :: AudioUnit) (audioUnitList :: AudioUnitList) (output :: Type) | foundAU audioUnit audioUnitList -> output

instance audioUnitInAudioUnitListTrue :: AudioUnitInAudioUnitList True a b True

instance audioUnitInAudioUnitListFalseNil :: AudioUnitInAudioUnitList False a AudioUnitNil False

instance audioUnitInAudioUnitListFalseCons ::
  ( AudioUnitEq au head foundAU
  , AudioUnitInAudioUnitList foundAU au tail o
  ) =>
  AudioUnitInAudioUnitList False au (AudioUnitCons head tail) o

-- | Assertion that all `pointers`  are present in a `nodeList`.
class AllPtrsInNodeList (pointers :: PtrList) (nodeList :: NodeList)

instance allPtrsInNodeList :: AllPtrsInNodeList PtrListNil haystack

instance allPtrsInNodeListCons ::
  ( LookupNL NodeListNil head haystack (NodeListCons x NodeListNil)
  , AllPtrsInNodeList tail haystack
  ) =>
  AllPtrsInNodeList (PtrListCons head tail) haystack

-- | For a given `node`, get all of its incoming edges as a `ptrList`.
class GetEdgesAsPtrList (node :: Node) (ptrList :: PtrList) | node -> ptrList

instance getEdgesAsPtrListNoEdge :: GetEdgesAsPtrList (NodeC x NoEdge) PtrListNil

instance getEdgesAsPtrListSingleEdge :: GetEdgesAsPtrList (NodeC x (SingleEdge e)) (PtrListCons e PtrListNil)

instance getEdgesAsPtrListManyEdges :: GetEdgesAsPtrList (NodeC x (ManyEdges e l)) (PtrListCons e l)

-- | For a given set of nodes called `needles`, assert that all of them are present in at least one incoming edge of the nodes called `haystack`.
class AllEdgesInNodeList (needles :: NodeList) (haystack :: NodeList)

instance allEdgesInNodeListNil :: AllEdgesInNodeList NodeListNil haystack

instance allEdgesInNodeListCons ::
  ( GetEdgesAsPtrList head ptrList
  , AllPtrsInNodeList ptrList haystack
  , AllEdgesInNodeList tail haystack
  ) =>
  AllEdgesInNodeList (NodeListCons head tail) haystack

-- | Assertion that all edges in the graph point to a node in the graph.
class AllEdgesPointToNodes (graph :: Graph)

instance allEdgesPointToNodes :: (GraphToNodeList graph nodeList, AllEdgesInNodeList nodeList nodeList) => AllEdgesPointToNodes graph

-- | Assertion that `ptr` is _not_ in `ptrList`.
class PtrNotInPtrList (ptr :: Ptr) (ptrList :: PtrList)

instance ptrNotInPtrListNil :: PtrNotInPtrList ptr PtrListNil

instance ptrNotInPtrListCons ::
  ( BinEq ptr head False
  , PtrNotInPtrList ptr tail
  ) =>
  PtrNotInPtrList ptr (PtrListCons head tail)

-- | Assertion that not pointers are duplicated in `ptrList`.
class NoPtrsAreDuplicatedInPtrList (ptrList :: PtrList)

instance noPtrsAreDuplicatedInPtrListNil :: NoPtrsAreDuplicatedInPtrList PtrListNil

instance noPtrsAreDuplicatedInPtrListCons ::
  ( PtrNotInPtrList head tail
  , NoPtrsAreDuplicatedInPtrList tail
  ) =>
  NoPtrsAreDuplicatedInPtrList (PtrListCons head tail)

-- | Assertion that `nodeList` contains no parallel edges. Parallel edges are two or more edges from `node1` to `node2` in a list of nodes.
class NoParallelEdgesNL (nodeList :: NodeList)

instance noParallelEdgesNLNil :: NoParallelEdgesNL NodeListNil

instance noParallelEdgesNLConsNoEdge :: (NoParallelEdgesNL tail) => NoParallelEdgesNL (NodeListCons (NodeC n NoEdge) tail)

instance noParallelEdgesNLConsSingleEdge :: (NoParallelEdgesNL tail) => NoParallelEdgesNL (NodeListCons (NodeC n (SingleEdge e)) tail)

instance noParallelEdgesNLConsManyEdges ::
  ( NoPtrsAreDuplicatedInPtrList (PtrListCons e l)
  , NoParallelEdgesNL tail
  ) =>
  NoParallelEdgesNL (NodeListCons (NodeC n (ManyEdges e l)) tail)

-- | Assertion that `graph` contains no parallel edges. Parallel edges are two or more edges from `node1` to `node2` in a list of nodes.
class NoParallelEdges (graph :: Graph)

instance noParallelEdges ::
  ( GraphToNodeList graph nodeList
  , NoParallelEdgesNL nodeList
  ) =>
  NoParallelEdges graph

-- | Tail-recursive lookup of all nodes in a graph without incoming edges.
-- | - `accumulator` is the accumulator that will be returned.
-- | - `nodeList` is the list of nodes that will be searched over.
-- | - `output` contains all nodes without incoming edges.
class SourceNodesNL (accumulator :: NodeList) (nodeList :: NodeList) (output :: NodeList) | accumulator nodeList -> output

instance sourceNodesNLNil :: SourceNodesNL accumulator NodeListNil accumulator

instance sourceNodesNLConsNoEdge :: SourceNodesNL (NodeListCons (NodeC i NoEdge) accumulator) tail o => SourceNodesNL accumulator (NodeListCons (NodeC i NoEdge) tail) o

instance sourceNodesNLConsSingleEdge :: SourceNodesNL accumulator tail o => SourceNodesNL accumulator (NodeListCons (NodeC i (SingleEdge x)) tail) o

instance sourceNodesNLConsManyEdges :: SourceNodesNL accumulator tail o => SourceNodesNL accumulator (NodeListCons (NodeC i (ManyEdges x l)) tail) o

-- | Lookup of all nodes in `graph` without incoming edges and return them as `nodeList`.
class SourceNodes (graph :: Graph) (nodeList :: NodeList) | graph -> nodeList

instance sourceNodes ::
  ( GraphToNodeList graph nodeList
  , SourceNodesNL NodeListNil nodeList sourceNodes
  ) =>
  SourceNodes graph sourceNodes

-- | Assertion that a graph has source nodes. Source nodes are nodes without incoming edges.
class HasSourceNodes (graph :: Graph)

instance hasSourceNodes :: SourceNodes graph (NodeListCons a b) => HasSourceNodes graph

-- | Tail-recursive lookup of audio unit in node list.
-- | - `foundNode`: an accumulator indicating whether we've found the node or not. Starts as False.
-- | - `audioUnit`: is this audioUnit in the node list?
-- | - `nodeList`: a list of nodes
-- | - `output`: `True` if the audio unit is in the list, `False` otherwise.
class AudioUnitInNodeList (foundNode :: Type) (audioUnit :: AudioUnit) (nodeList :: NodeList) (output :: Type) | foundNode audioUnit nodeList -> output

instance audioUnitInNodeListTrue :: AudioUnitInNodeList True a b True

instance audioUnitInNodeListFalseNil :: AudioUnitInNodeList False a NodeListNil False

instance audioUnitInNodeListFalseCons ::
  ( GetAudioUnit head headAu
  , AudioUnitEq au headAu foundNode
  , AudioUnitInNodeList foundNode au tail o
  ) =>
  AudioUnitInNodeList False au (NodeListCons head tail) o

-- | Tail-recursive removal of duplicate nodes from a node list.
-- | - `accumulator`: the accumulating deduped list. Starts at NodeListNil.
-- | - `maybeWithDuplicates`: a node list that potentially has duplicates.
-- | - `output`: the final deduped list
class RemoveDuplicates (accumulator :: NodeList) (maybeWithDuplicates :: NodeList) (output :: NodeList) | accumulator maybeWithDuplicates -> output

instance removeDuplicatesNil :: RemoveDuplicates accumulator NodeListNil accumulator

instance removeDuplicatesCons ::
  ( GetAudioUnit head au
  , AudioUnitInNodeList False au accumulator tf
  , Gate tf accumulator (NodeListCons head accumulator) acc
  , RemoveDuplicates acc tail o
  ) =>
  RemoveDuplicates accumulator (NodeListCons head tail) o

-- | Tail-recursive lookup of node in a node list.
-- | - `foundNode`: an accumulator indicating whether we've found the node or not. Starts as False.
-- | - `node`: is this node in the node list?
-- | - `nodeList`: a list of nodes
-- | - `output`: `True` if the node is in the list, `False` otherwise.
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

-- | Tail-recursive algorithm to find nodes we have not visited yet.
-- | - `visited` nodes that we have visited
-- | - `accumulator` accumulates unvisited nodes
-- | - `candidates` are nodes that have potentially not been visited yet and will be cross-referenced against `visited`.
-- | - `unvisited` is a list of unvisited nodes.
class UnvisitedNodes (visited :: NodeList) (accumulator :: NodeList) (candidates :: NodeList) (unvisited :: NodeList) | visited accumulator candidates -> unvisited

instance unvisitedNodesNil :: UnvisitedNodes visited accumulator NodeListNil accumulator

instance unvisitedNodesCons ::
  ( NodeInNodeList False head visited tf
  , Gate tf accumulator (NodeListCons head accumulator) acc
  , UnvisitedNodes visited acc tail o
  ) =>
  UnvisitedNodes visited accumulator (NodeListCons head tail) o

-- | In the node-visiting algorithm, this is a single step where we
-- | look for a node in a given list of edges. This step is repeated until
-- | all of the nodes have been visited.
-- |
-- | - `accumulator` - the running accumulator of nodes we are visiting
-- | - `graph` - the entire graph. if we were using an env comonad, this would be the environment
-- | - `findMeInAnEdge` - this is the node we want to find in edges
-- | - `candidates` - these are all of the nodes that have `findMeInAnEdge` as an incoming connection
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

-- | This appends node list `l` and `r` to produce the result `o`.
class NodeListAppend (l :: NodeList) (r :: NodeList) (o :: NodeList) | l r -> o

instance nodeListAppendNilNil :: NodeListAppend NodeListNil NodeListNil NodeListNil

instance nodeListAppendNilL :: NodeListAppend NodeListNil (NodeListCons x y) (NodeListCons x y)

instance nodeListAppendNilR :: NodeListAppend (NodeListCons x y) NodeListNil (NodeListCons x y)

instance nodeListAppendCons :: (NodeListAppend b (NodeListCons c d) o) => NodeListAppend (NodeListCons a b) (NodeListCons c d) (NodeListCons a o)

-- | This appends pointer list `l` and `r` to produce the result `o`.
class PtrListAppend (l :: PtrList) (r :: PtrList) (o :: PtrList) | l r -> o

instance ptrListAppendNilNil :: PtrListAppend PtrListNil PtrListNil PtrListNil

instance ptrListAppendNilL :: PtrListAppend PtrListNil (PtrListCons x y) (PtrListCons x y)

instance ptrListAppendNilR :: PtrListAppend (PtrListCons x y) PtrListNil (PtrListCons x y)

instance ptrListAppendCons :: (PtrListAppend b (PtrListCons c d) o) => PtrListAppend (PtrListCons a b) (PtrListCons c d) (PtrListCons a o)

-- | This is the equivalent of `Alt` for edge profiles. `NoEdge` always loses.
class AltEdgeProfile (a :: EdgeProfile) (b :: EdgeProfile) (c :: EdgeProfile) | a b -> c

instance edgeProfileChooseGreater0 :: AltEdgeProfile NoEdge b b
else instance edgeProfileChooseGreater1 :: AltEdgeProfile a NoEdge a

-- | Asks is `nodeList` empty and replies with `True` or `False` as `tf`.
class IsNodeListEmpty (nodeList :: NodeList) (tf :: Type) | nodeList -> tf

instance isNodeListEmptyNil :: IsNodeListEmpty NodeListNil True

instance isNodeListEmptyCons :: IsNodeListEmpty (NodeListCons a b) False

-- | In the visiting algorithm, obtains a list of nodes to visit
-- | as well as parentless or "sink" nodes.
-- | - `candidatesAccumulator` an accumulator of candidates to visit
-- | - `parentlessAccumulator` an accumulator of parentless or "sink" nodes
-- | - `graph` - the entire graph. if we were using an env comonad, this would be the environment
-- | - `findMeInAnEdge` - a list of nodes remaining to find in edges. This starts as the whole graph and whittles down to no nodes as the algorithm runs.
-- | - `candidates` return value of candidates for the next step
-- | - `parentless` return value of parentless nodes in the graph
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

-- | The algorithm that finds the terminal nodes of a graph.
-- | - `graph` - the entire graph. if we were using an env comonad, this would be the environment
-- | - `visited` - nodes we have visited so far in the aglorithm
-- | - `visiting` - nodes we are currently visiting in the aglorithm
-- | - `accumulator` - accumulator for tail-recursion that builds output
-- | - `output` - terminal nodes of the graph
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

-- | Assertion that `graph` has the unique terminus `node`. Note that,
-- | due to the fundep, the assertion also _finds_ the unique terminus
-- | if it exists.
class UniqueTerminus (graph :: Graph) (node :: Node) | graph -> node

instance uniqueTerminus ::
  ( SourceNodes graph source
  , GraphToNodeList graph graphAsNodeList
  , TerminusLoop graphAsNodeList NodeListNil source NodeListNil terminii
  -- we remove duplicates from the parent only after terminus loop, as we are not recursing over it in the loop
  , RemoveDuplicates NodeListNil terminii (NodeListCons node NodeListNil)
  ) =>
  UniqueTerminus graph node

-- | Assertion that `graph` has a unique terminus.
class HasUniqueTerminus (graph :: Graph)

instance hasUniqueTerminus :: UniqueTerminus graph node => HasUniqueTerminus graph

-- | Asserts that all nodes in `graph` are saturated.
-- | "Saturation" for audio nodes is similar to the concept of saturation
-- | in types, namely that they have all the information necessary to
-- | render. In doing so, it also fails the assertion on degenerate nodes,
-- | ie sine-wave oscillators with input (which wouldn't make sense).
class AllNodesAreSaturatedNL (graph :: NodeList)

instance allNodesAreSaturatedNil :: AllNodesAreSaturatedNL NodeListNil

instance allNodesAreSaturatedConsTAllpass :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TAllpass a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTBandpass :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TBandpass a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTConstant :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TConstant a) NoEdge) tail)

instance allNodesAreSaturatedConsTConvolver :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TConvolver a name) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTDelay :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TDelay a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTDynamicsCompressor :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TDynamicsCompressor a) (SingleEdge e)) tail)

instance allNodesAreSaturatedCons_SE_TGain :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TGain a) (SingleEdge e)) tail)

instance allNodesAreSaturatedCons_ME_TGain :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TGain a) (ManyEdges e l)) tail)

instance allNodesAreSaturatedConsTHighpass :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.THighpass a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTHighshelf :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.THighshelf a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTLoopBuf :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TLoopBuf a name) NoEdge) tail)

instance allNodesAreSaturatedConsTLowpass :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TLowpass a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTLowshelf :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TLowshelf a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTMicrophone :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TMicrophone a) NoEdge) tail)

instance allNodesAreSaturatedConsTNotch :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TNotch a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTPeaking :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TPeaking a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTPeriodicOsc :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TPeriodicOsc a name) NoEdge) tail)

instance allNodesAreSaturatedConsTPlayBuf :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TPlayBuf a name) NoEdge) tail)

instance allNodesAreSaturatedConsTRecorder :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TRecorder a name) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTSawtoothOsc :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TSawtoothOsc a) NoEdge) tail)

instance allNodesAreSaturatedConsTSinOsc :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TSinOsc a) NoEdge) tail)

instance allNodesAreSaturatedCons_SE_TSpeaker :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TSpeaker a) (SingleEdge e)) tail)

instance allNodesAreSaturatedCons_ME_TSpeaker :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TSpeaker a) (ManyEdges e l)) tail)

instance allNodesAreSaturatedConsTSquareOsc :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TSquareOsc a) NoEdge) tail)

instance allNodesAreSaturatedConsTStereoPanner :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TStereoPanner a) (SingleEdge e)) tail)

instance allNodesAreSaturatedConsTTriangleOsc :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TTriangleOsc a) NoEdge) tail)

instance allNodesAreSaturatedConsTWaveShaper :: AllNodesAreSaturatedNL tail => AllNodesAreSaturatedNL (NodeListCons (NodeC (AU.TWaveShaper a name) (SingleEdge e)) tail)

-- | Asserts that all nodes in `graph` are saturated. This is
-- | the same as the node-list algorithm, but for a graph.
class AllNodesAreSaturated (graph :: Graph)

instance allNodesAreSaturated :: (GraphToNodeList graph nodeList, AllNodesAreSaturatedNL nodeList) => AllNodesAreSaturated graph

-- | Asserts the node is an output device. Currently, in the web-audio API,
-- | this is a loudspeaker or recorder.
class NodeIsOutputDevice (node :: Node)

instance nodeIsOutputDeviceTSpeaker :: NodeIsOutputDevice (NodeC (AU.TSpeaker a) x)

instance nodeIsOutputDeviceTRecorder :: NodeIsOutputDevice (NodeC (AU.TRecorder a name) x)
