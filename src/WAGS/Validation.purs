-- | Validation algorithms for audio graphs.
-- |
-- | Validation includes making sure the graph is renderable by
-- | the web audio API and retrieving information about valid graphs,
-- | like their terminal node (ie a loudspeaker).
module WAGS.Validation where

import Prelude hiding (Ordering(..))

import Data.Typelevel.Bool (False, True)
import Data.Typelevel.Num (class Pred, D0)
import Prim.Row (class Nub)
import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Edge (EdgeList)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC, NodeList)
import WAGS.Util (class Gate, class RowListEmpty, class SymInRowList)

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
  , UniqueTerminus graph name terminus
  , NodeIsOutputDevice terminus
  , AllNodesAreSaturated graph
  ) =>
  GraphIsRenderable graph

-- | Retrieves the terminal node from an audio grpah. This is
-- | almost always a speaker or recording, but if the graph is
-- | at an intermediary stage of construction, this could be another
-- | top-level node, ie a gain node.
class TerminalNodeC (g :: Graph) (ptr :: Symbol) | g -> ptr

instance terminalNodeC ::
  ( UniqueTerminus g sym t
  ) =>
  TerminalNodeC g sym

class NoNodesAreDuplicated (graph :: Graph)

instance noNodesAreDuplicated :: Nub graph graph => NoNodesAreDuplicated graph

-- | Assert that all edges are present as nodes in a graph
class AllEdgesInGraph' (edgeProfile :: EdgeList) (graph :: Graph)

instance allEdgesInGraph'Nil :: AllEdgesInGraph' RL.Nil graph

instance allEdgesInGraph'Cons ::
  ( R.Cons sym node graph' graph
  , AllEdgesInGraph' rest graph
  ) =>
  AllEdgesInGraph' (RL.Cons sym Unit rest) graph

-- | Assert that all edges are present as nodes in a graph
class AllEdgesInGraph (graphAsList :: NodeList) (graph :: Graph)

instance allEdgesInGraphNil :: AllEdgesInGraph RL.Nil graph

instance allEdgesInGraphCons ::
  ( RowToList edges edges'
  , AllEdgesInGraph' edges' graph
  , AllEdgesInGraph rest graph
  ) =>
  AllEdgesInGraph (RL.Cons sym (NodeC node { | edges }) rest) graph

-- | Assertion that all edges in the graph point to a node in the graph.
class AllEdgesPointToNodes (graph :: Graph)

instance allEdgesPointToNodes :: (RowToList graph nodeList, AllEdgesInGraph nodeList graph) => AllEdgesPointToNodes graph

-- | Assertion that `nodeList` contains no parallel edges. Parallel edges are two or more edges from `node1` to `node2` in a list of nodes.
class NoParallelEdgesNL (nodeList :: NodeList)

instance noParallelEdgesNLNil :: NoParallelEdgesNL RL.Nil

instance noParallelEdgesNLConsManyEdges :: Nub r r => NoParallelEdgesNL (RL.Cons h (NodeC n { | r }) tail)

-- | Assertion that `graph` contains no parallel edges. Parallel edges are two or more edges from `node1` to `node2` in a list of nodes.
class NoParallelEdges (graph :: Graph)

instance noParallelEdges ::
  ( RowToList graph nodeList
  , NoParallelEdgesNL nodeList
  ) =>
  NoParallelEdges graph

-- | Tail-recursive lookup of all nodes in a graph without incoming edges.
-- | - `nodeList` is the list of nodes that will be searched over.
-- | - `output` contains all nodes without incoming edges.
class SourceNodes (nodeList :: NodeList) (output :: NodeList) | nodeList -> output

instance sourceNodesNil :: SourceNodes RL.Nil RL.Nil

instance sourceNodesConsNil ::
  ( RowToList edges edges'
  , RowListEmpty edges' tf
  , SourceNodes rest out'
  , Gate tf (RL.Cons sym (NodeC node { | edges }) out') out' out
  ) =>
  SourceNodes (RL.Cons sym (NodeC node { | edges }) rest) out

-- | Assertion that a graph has source nodes. Source nodes are nodes without incoming edges.
class HasSourceNodes (graph :: Graph)

instance hasSourceNodes :: (RowToList graph graph', SourceNodes graph' (RL.Cons a b c)) => HasSourceNodes graph

class SymPresentInAtLeastOneEdge (tf :: Type) (sym :: Symbol) (graphAsList :: RowList Type) (o :: Type) | tf sym graphAsList -> o

instance symPresentInAtLeastOneEdgeTrue :: SymPresentInAtLeastOneEdge True sym graphAsList True

instance symPresentInAtLeastOneEdgeNil :: SymPresentInAtLeastOneEdge False sym RL.Nil False

instance symPresentInAtLeastOneEdgeCons ::
  ( RowToList edges edges'
  , SymInRowList sym edges' tf
  , SymPresentInAtLeastOneEdge tf sym rest o
  ) =>
  SymPresentInAtLeastOneEdge False sym (RL.Cons sym' (NodeC node { | edges }) rest) o

-- | The algorithm that finds the terminal nodes of a graph.
class GetTerminii (toVisit :: NodeList) (graphAsNodeList :: NodeList) (output :: NodeList) | toVisit graphAsNodeList -> output

instance getTerminiiNil :: GetTerminii RL.Nil graphAsNodeList RL.Nil

instance getTerminiiCons ::
  ( SymPresentInAtLeastOneEdge False sym graphAsList tf
  , GetTerminii rest graphAsList o
  , Gate tf o (RL.Cons sym node o) oo
  ) =>
  GetTerminii (RL.Cons sym node rest) graphAsList oo

-- | Assertion that `graph` has the unique terminus `node`. Note that,
-- | due to the fundep, the assertion also _finds_ the unique terminus
-- | if it exists.
class UniqueTerminus (graph :: Graph) (sym :: Symbol) (node :: Type) | graph -> sym node

instance uniqueTerminus ::
  ( RowToList graph graphAsList
  , GetTerminii graphAsList graphAsList (RL.Cons sym node RL.Nil)
  ) =>
  UniqueTerminus graph sym node

-- | Assertion that `graph` has a unique terminus.
class HasUniqueTerminus (graph :: Graph)

instance hasUniqueTerminus :: UniqueTerminus graph sym node => HasUniqueTerminus graph

-- | Asserts that all nodes in `graph` are saturated.
-- | "Saturation" for audio nodes is similar to the concept of saturation
-- | in types, namely that they have all the information necessary to
-- | render. In doing so, it also fails the assertion on degenerate nodes,
-- | ie sine-wave oscillators with input (which wouldn't make sense).
class AllNodesAreSaturatedNL (graph :: NodeList)

instance allNodesAreSaturatedNil :: AllNodesAreSaturatedNL RL.Nil

instance allNodesAreSaturatedConsTAllpass ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TAllpass) { | r }) tail)

instance allNodesAreSaturatedConsTAnalyser ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TAnalyser p0) { | r }) tail)

class Length (rl :: RowList Type) (n :: Type) | rl -> n

instance length0 :: Length RL.Nil D0
else instance lengthN :: (Pred n nMinus1, Length rest nMinus1) => Length (RL.Cons a b rest) nMinus1

instance allNodesAreSaturatedConsTAudioWorkletNode0 ::
  ( RowToList r rl
  , Length rl numberOfInputs
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TAudioWorkletNode sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions) { | r }) tail)

instance allNodesAreSaturatedConsTBandpass2 ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TBandpass) { | r }) tail)

instance allNodesAreSaturatedConsTConstant ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TConstant) { | r }) tail)

instance allNodesAreSaturatedConsTConvolver ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TConvolver p0) { | r }) tail)

instance allNodesAreSaturatedConsTDelay ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TDelay) { | r }) tail)

instance allNodesAreSaturatedConsTDynamicsCompressor ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TDynamicsCompressor) { | r }) tail)

instance allNodesAreSaturatedCons_TGain ::
  ( RowToList r (RL.Cons aSym aVal next)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TGain) { | r }) tail)

instance allNodesAreSaturatedConsTHighpass ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.THighpass) { | r }) tail)

instance allNodesAreSaturatedConsTHighshelf ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.THighshelf) { | r }) tail)

instance allNodesAreSaturatedConsTLoopBuf ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TLoopBuf) { | r }) tail)

instance allNodesAreSaturatedConsTLowpass ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TLowpass) { | r }) tail)

instance allNodesAreSaturatedConsTLowshelf ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TLowshelf) { | r }) tail)

instance allNodesAreSaturatedConsTMicrophone ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC CTOR.TMicrophone { | r }) tail)

instance allNodesAreSaturatedConsTNotch ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TNotch) { | r }) tail)

instance allNodesAreSaturatedConsTPeaking ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TPeaking) { | r }) tail)

instance allNodesAreSaturatedConsTPeriodicOsc ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TPeriodicOsc) { | r }) tail)

instance allNodesAreSaturatedConsTPlayBuf ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TPlayBuf) { | r }) tail)

instance allNodesAreSaturatedConsTRecorder ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TRecorder p0) { | r }) tail)

instance allNodesAreSaturatedConsTSawtoothOsc ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TSawtoothOsc) { | r }) tail)

instance allNodesAreSaturatedConsTSinOsc ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TSinOsc) { | r }) tail)

instance allNodesAreSaturatedCons_SE_TSpeaker ::
  ( RowToList r (RL.Cons aSym aVal next)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC CTOR.TSpeaker { | r }) tail)

instance allNodesAreSaturatedConsTSquareOsc ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TSquareOsc) { | r }) tail)

instance allNodesAreSaturatedConsTStereoPanner ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TStereoPanner) { | r }) tail)

instance allNodesAreSaturatedConsTTriangleOsc ::
  ( RowToList r RL.Nil
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TTriangleOsc) { | r }) tail)

instance allNodesAreSaturatedConsTWaveShaper ::
  ( RowToList r (RL.Cons aSym aVal RL.Nil)
  , AllNodesAreSaturatedNL tail
  ) =>
  AllNodesAreSaturatedNL (RL.Cons iSym (NodeC (CTOR.TWaveShaper p0 p1) { | r }) tail)

-- | Asserts that all nodes in `graph` are saturated. This is
-- | the same as the node-list algorithm, but for a graph.
class AllNodesAreSaturated (graph :: Graph)

instance allNodesAreSaturated :: (RowToList graph nodeList, AllNodesAreSaturatedNL nodeList) => AllNodesAreSaturated graph

-- | Asserts the node is an output device. Currently, in the web-audio API,
-- | this is a loudspeaker or recorder.
class NodeIsOutputDevice (node :: Type)

instance nodeIsOutputDeviceTSpeaker :: NodeIsOutputDevice (NodeC (CTOR.TSpeaker) x)

instance nodeIsOutputDeviceTRecorder :: NodeIsOutputDevice (NodeC (CTOR.TRecorder name) x)
