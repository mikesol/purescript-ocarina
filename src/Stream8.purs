module Stream8 where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Lazy (class Lazy)
import Control.Monad.Cont (class MonadCont)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadTrans, State, StateT, get, gets, modify_)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus, class MonadZero)
import Control.Plus (class Plus)
import Data.Functor.Indexed (class IxFunctor)
import Data.Identity (Identity)
import Data.Map (Map, insert)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Bool (False, True)
import Data.Typelevel.Num (class Nat, class Succ, D0, d0, toInt')
import Effect.Class (class MonadEffect)
import Prim.TypeError (class Warn, Above, Beside, Quote, Text)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

infixr 5 type Above as ^^

infixr 5 type NodeListCons as /:

infixr 5 type PtrListCons as +:

infixr 5 type NodeC as /->

data Binary

foreign import data I :: Binary

foreign import data O :: Binary

type Ptr
  = Type

data TAudioParameter

foreign import data Changing :: TAudioParameter

foreign import data Static :: TAudioParameter

data AudioUnitList

foreign import data AudioUnitCons :: AudioUnit -> AudioUnitList -> AudioUnitList

foreign import data AudioUnitNil :: AudioUnitList

data PtrList

foreign import data PtrListCons :: Ptr -> PtrList -> PtrList

foreign import data PtrListNil :: PtrList

data EdgeProfile

-- non empty
foreign import data ManyEdges :: Ptr -> PtrList -> EdgeProfile

foreign import data SingleEdge :: Ptr -> EdgeProfile

foreign import data NoEdge :: EdgeProfile

data AudioUnit

foreign import data TSinOsc :: Ptr -> TAudioParameter -> AudioUnit

foreign import data THighpass :: Ptr -> TAudioParameter -> TAudioParameter -> AudioUnit

foreign import data TGain :: Ptr -> TAudioParameter -> AudioUnit

foreign import data TSpeaker :: Ptr -> AudioUnit

data Node

foreign import data NodeC :: AudioUnit -> EdgeProfile -> Node

data NodeList

foreign import data NodeListCons :: Node -> NodeList -> NodeList

foreign import data NodeListNil :: NodeList

data Graph

-- non empty
foreign import data GraphC :: Node -> NodeList -> Graph

data Universe

-- currentIdx graph destroyable accumulator
foreign import data UniverseC :: Ptr -> Graph -> NodeList -> Type -> Universe

---------------------------
------------ util
class GetAccumulator (u :: Universe) (acc :: Type) | u -> acc

instance getAccumulator :: GetAccumulator (UniverseC ptr graph destroyable acc) acc

class GetGraph (u :: Universe) (g :: Graph) | u -> g

instance getGraphUniverseC :: GetGraph (UniverseC ptr graph destroyed acc) graph

class GetPointer (audioUnit :: AudioUnit) (ptr :: Ptr) | audioUnit -> ptr

instance getPointerSinOsc :: GetPointer (TSinOsc ptr a) ptr

instance getPointerHighpass :: GetPointer (THighpass ptr a b) ptr

instance getPointerGain :: GetPointer (TGain ptr a) ptr

instance getPointerSpeaker :: GetPointer (TSpeaker ptr) ptr

class Gate tf l r o | tf l r -> o

instance gateTrue :: Gate True l r l

instance gateFalse :: Gate False l r r

class GraphToNodeList (graph :: Graph) (nodeList :: NodeList) | graph -> nodeList, nodeList -> graph

instance graphToNodeList :: GraphToNodeList (GraphC node nodeList) (NodeListCons node nodeList)

class GetAudioUnit (node :: Node) (au :: AudioUnit) | node -> au

instance getAudioUnitNodeC :: GetAudioUnit (NodeC au ep) au

class PtrEq (a :: Ptr) (b :: Ptr) (tf :: Type) | a b -> tf

instance ptrEqTrue :: PtrEq a a True
else instance ptrEqFalse :: PtrEq a b False

class AudioUnitEq (a :: AudioUnit) (b :: AudioUnit) (tf :: Type) | a b -> tf

instance audioUnitEqTSinOsc :: AudioUnitEq (TSinOsc idx freq) (TSinOsc idx freq) True
else instance audioUnitEqTHighpass :: AudioUnitEq (THighpass idx freq q) (THighpass idx freq q) True
else instance audioUnitEqTGain :: AudioUnitEq (TGain idx vol) (TGain idx vol) True
else instance audioUnitEqTSpeaker :: AudioUnitEq (TSpeaker idx) (TSpeaker idx) True
else instance audioUnitEqFalse :: AudioUnitEq a b False

class TermToInitialAudioUnit (a :: Type) (p :: Ptr) (b :: AudioUnit) | a p -> b

instance termToInitialAudioUnitSinOsc :: TermToInitialAudioUnit (SinOsc a) ptr (TSinOsc ptr Changing)

instance termToInitialAudioUnitHighpass :: TermToInitialAudioUnit (Highpass a b c) ptr (THighpass ptr Changing Changing)

instance termToInitialAudioUnitGain :: TermToInitialAudioUnit (Gain a b) ptr (TGain ptr Changing)

instance termToInitialAudioUnitSpeaker :: TermToInitialAudioUnit (Speaker a) ptr (TSpeaker ptr)

class CreationInstructions (env :: Type) (acc :: Type) (g :: Type) where
  creationInstructions :: Int -> env -> acc -> g -> Array Instruction /\ AnAudioUnit

instance creationInstructionsSinOsc :: InitialVal env acc a => CreationInstructions env acc (SinOsc a) where
  creationInstructions idx env acc (SinOsc a) =
    let
      iv' = initialVal env acc a

      AudioParameter iv = iv'
    in
      [ NewUnit idx "sinosc"
      , SetFrequency idx iv.param iv.timeOffset iv.transition
      ]
        /\ ASinOsc iv'

instance creationInstructionsHighpass :: (InitialVal env acc a, InitialVal env acc b) => CreationInstructions env acc (Highpass a b c) where
  creationInstructions idx env acc (Highpass a b _) =
    let
      aiv' = initialVal env acc a

      biv' = initialVal env acc b

      AudioParameter aiv = aiv'

      AudioParameter biv = biv'
    in
      [ NewUnit idx "highpass"
      , SetFrequency idx aiv.param aiv.timeOffset aiv.transition
      , SetQ idx biv.param biv.timeOffset biv.transition
      ]
        /\ AHighpass aiv' biv'

instance creationInstructionsGain :: InitialVal env acc a => CreationInstructions env acc (Gain a b) where
  creationInstructions idx env acc (Gain a _) =
    let
      iv' = initialVal env acc a

      AudioParameter iv = iv'
    in
      [ NewUnit idx "gain"
      , SetGain idx iv.param iv.timeOffset iv.transition
      ]
        /\ AGain iv'

instance creationInstructionsSpeaker :: CreationInstructions env acc (Speaker a) where
  creationInstructions idx env acc (Speaker _) = [] /\ ASpeaker

class NodeListKeepSingleton (nodeListA :: NodeList) (nodeListB :: NodeList) (nodeListC :: NodeList) | nodeListA nodeListB -> nodeListC

instance nodeListKeepSingletonNil :: NodeListKeepSingleton NodeListNil NodeListNil NodeListNil

instance nodeListKeepSingletonL :: NodeListKeepSingleton (NodeListCons a NodeListNil) NodeListNil (NodeListCons a NodeListNil)

instance nodeListKeepSingletonR :: NodeListKeepSingleton NodeListNil (NodeListCons a NodeListNil) (NodeListCons a NodeListNil)

class LookupNL (accumulator :: NodeList) (ptr :: Ptr) (graph :: NodeList) (node :: NodeList) | ptr graph -> node

instance lookupNLNil :: LookupNL accumulator ptr NodeListNil accumulator

instance lookupNLNilCons :: (GetAudioUnit head headAU, GetPointer headAU maybePtr, PtrEq maybePtr ptr tf, Gate tf (NodeListCons head NodeListNil) NodeListNil toComp, NodeListKeepSingleton toComp accumulator acc, LookupNL acc ptr tail o) => LookupNL accumulator ptr (NodeListCons head tail) o

class Lookup (ptr :: Ptr) (graph :: Graph) (node :: Node) | ptr graph -> node

instance lookup :: (GraphToNodeList graph nodeList, LookupNL NodeListNil ptr nodeList (NodeListCons node NodeListNil)) => Lookup ptr graph node

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
  ( PtrEq ptr head foundNode
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
  ( PtrEq ptr head False
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

instance allNodesAreFullyHydratedConsTSinOsc :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TSinOsc a b) NoEdge) tail)

instance allNodesAreFullyHydratedConsTHighpass :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (THighpass a b c) (SingleEdge e)) tail)

instance allNodesAreFullyHydratedConsTGainME :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TGain a b) (ManyEdges e l)) tail)

instance allNodesAreFullyHydratedConsTGainSE :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TGain a b) (SingleEdge e)) tail)

instance allNodesAreFullyHydratedConsTSpeaker :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TSpeaker a) (ManyEdges e l)) tail)

class AllNodesAreFullyHydrated (graph :: Graph)

instance allNodesAreFullyHydrated :: (GraphToNodeList graph nodeList, AllNodesAreFullyHydratedNL nodeList) => AllNodesAreFullyHydrated graph

class NodeIsOutputDevice (node :: Node)

instance nodeIsOutputDeviceTSpeaker :: NodeIsOutputDevice (NodeC (TSpeaker a) x)

-- for the end
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

-- for any given step
class GraphIsCoherent (graph :: Graph)

instance graphIsCoherent ::
  ( NoNodesAreDuplicated graph
  , AllEdgesPointToNodes graph
  , NoParallelEdges graph
  ) =>
  GraphIsCoherent graph

-- create (hpf and gain can start empty)
-- get (uses a getter on a node to get another node, think optics)
-- remove (leaves a hole, no attempt to reconstitute chain)
-- destroy (destroys all connected nodes)
-- replace (for hpf and gain)
-- add (for gain and hpf)
data AudioParameterTransition
  = NoRamp
  | LinearRamp
  | ExponentialRamp
  | Immediately

data Instruction
  = Stop Int
  | Free Int
  | DisconnectXFromY Int Int -- id id
  | ConnectXToY Int Int
  | NewUnit Int String
  | SetFrequency Int Number Number AudioParameterTransition -- frequency
  | SetThreshold Int Number Number AudioParameterTransition -- threshold
  | SetKnee Int Number Number AudioParameterTransition -- knee
  | SetRatio Int Number Number AudioParameterTransition -- ratio
  | SetAttack Int Number Number AudioParameterTransition -- attack
  | SetRelease Int Number Number AudioParameterTransition -- release
  | SetBuffer Int Int (Array (Array Number)) -- buffer
  | SetQ Int Number Number AudioParameterTransition -- q
  | SetPlaybackRate Int Number Number AudioParameterTransition -- playback rate
  | SetPeriodicWave Int (Array Number) (Array Number) -- periodic wave
  | SetCurve Int (Array Number) -- curve
  | SetOversample Int String -- oversample
  | SetLoopStart Int Number Boolean -- loop start
  | SetLoopEnd Int Number Boolean -- loop end
  | SetPan Int Number Number AudioParameterTransition -- pan for pan node
  | SetGain Int Number Number AudioParameterTransition -- gain for gain node, boolean if is start
  | SetDelay Int Number Number AudioParameterTransition -- delay for delay node
  | SetOffset Int Number Number AudioParameterTransition -- offset for const node
  | SetCustomParam Int String Number Number AudioParameterTransition -- for audio worklet nodes
  | SetConeInnerAngle Int Number
  | SetConeOuterAngle Int Number
  | SetConeOuterGain Int Number
  | SetDistanceModel Int String
  | SetMaxDistance Int Number
  | SetOrientationX Int Number Number AudioParameterTransition
  | SetOrientationY Int Number Number AudioParameterTransition
  | SetOrientationZ Int Number Number AudioParameterTransition
  | SetPanningModel Int String
  | SetPositionX Int Number Number AudioParameterTransition
  | SetPositionY Int Number Number AudioParameterTransition
  | SetPositionZ Int Number Number AudioParameterTransition
  | SetRefDistance Int Number
  | SetRolloffFactor Int Number

type AudioState env acc a
  = State
      { env :: env
      , acc :: acc
      , currentIdx :: Int
      , instructions :: Array Instruction
      , internalGraph :: Map Int AnAudioUnit
      }
      a

newtype Scene (env :: Type) (acc :: Type) (ig :: Universe) (og :: Universe) (a :: Type)
  = Scene (AudioState env acc a)

-- do not export!
unScene :: forall env acc i o a. Scene env acc i o a -> AudioState env acc a
unScene (Scene state) = state

instance sceneIxFunctor :: IxFunctor (Scene env acc) where
  imap f (Scene a) = Scene (f <$> a)

instance sceneIxApplicative :: IxApply (Scene env acc) where
  iapply (Scene f) (Scene a) = Scene (f <*> a)

instance sceneIxApply :: IxApplicative (Scene env acc) where
  ipure a = Scene $ pure a

instance sceneIxBind :: IxBind (Scene env acc) where
  ibind (Scene monad) function = Scene (monad >>= (unScene <<< function))

instance sceneIxMonad :: IxMonad (Scene env acc)

-- create (hpf and gain can start empty)
defaultParam :: AudioParameter'
defaultParam = { param: 0.0, timeOffset: 0.0, transition: LinearRamp, forceSet: false }

type AudioParameter'
  = { param :: Number
    , timeOffset :: Number
    , transition :: AudioParameterTransition
    , forceSet :: Boolean
    }

newtype AudioParameter
  = AudioParameter AudioParameter'

class InitialVal env acc a where
  initialVal :: env -> acc -> a -> AudioParameter

instance initialValNumber :: InitialVal env acc Number where
  initialVal _ _ a = AudioParameter $ defaultParam { param = a }

instance initialValAudioParameter :: InitialVal env acc AudioParameter where
  initialVal _ _ = identity

instance initialValFunction :: InitialVal env acc (env -> acc -> AudioParameter) where
  initialVal env acc f = f env acc

instance initialValTuple :: InitialVal env acc a => InitialVal env acc (Tuple a b) where
  initialVal env acc a = initialVal env acc $ fst a

class SetterVal env acc a where
  setterVal :: a -> (env -> acc -> AudioParameter -> AudioParameter)

instance setterValNumber :: SetterVal env acc Number where
  setterVal = const <<< initialVal

instance setterValAudioParameter :: SetterVal env acc AudioParameter where
  setterVal = const <<< initialVal

instance setterValTuple :: SetterVal env acc (Tuple a (env -> acc -> AudioParameter -> AudioParameter)) where
  setterVal = snd

class IsChanging (b :: TAudioParameter) where
  isChanging :: Proxy b -> Boolean

class SetterVal env acc a <= SetterAsChanged env acc a (b :: TAudioParameter) | a -> b

instance setterAsChangedNumber :: SetterAsChanged env acc Number Static

instance setterAsChangedAudioParameter :: SetterAsChanged env acc AudioParameter Static

instance setterAsChangedTuple :: SetterAsChanged env acc (Tuple a (env -> acc -> AudioParameter -> AudioParameter)) Changing

data AudioUnitRef (ptr :: Ptr)
  = AudioUnitRef Int

data ARef
  = ARef

data SinOsc a
  = SinOsc a

data AnAudioUnit
  = ASinOsc AudioParameter
  | AHighpass AudioParameter AudioParameter
  | AGain AudioParameter
  | ASpeaker

data Highpass a b c
  = Highpass a b c

data Highpass_ a b
  = Highpass_ a b

data Gain a b
  = Gain a b

data Speaker a
  = Speaker a

class EdgeListable a (b :: PtrList) | a -> b where
  getPointers' :: a -> PtrArr b

instance edgeListableUnit :: EdgeListable Unit PtrListNil where
  getPointers' _ = PtrArr []

instance edgeListableTuple :: EdgeListable x y => EdgeListable (Tuple (AudioUnitRef ptr) x) (PtrListCons ptr y) where
  getPointers' (Tuple (AudioUnitRef i) x) = let PtrArr o = getPointers' x in PtrArr ([ i ] <> o)

newtype PtrArr a
  = PtrArr (Array Int)

class GetARefFunction (a :: Type) (b :: Type) | a -> b where
  getARefFunction :: a -> (ARef -> b)

class ToARefFunction (a :: Type) (b :: Type) | a -> b where
  toARefFunction :: a -> (ARef -> b)

instance getARefFunctionHighpass :: ToARefFunction i o => GetARefFunction (Highpass a b i) o where
  getARefFunction (Highpass a b c) = toARefFunction c

instance getARefFunctionGain :: ToARefFunction i o => GetARefFunction (Gain a i) o where
  getARefFunction (Gain a b) = toARefFunction b

instance getARefFunctionSpeaker :: ToARefFunction i o => GetARefFunction (Speaker i) o where
  getARefFunction (Speaker a) = toARefFunction a

instance toARefFunctionFunction :: ToARefFunction (ARef -> b) b where
  toARefFunction = identity
else instance toARefFunctionConst :: ToARefFunction b b where
  toARefFunction = const

class AsEdgeProfile a (b :: EdgeProfile) | a -> b where
  getPointers :: a -> PtrArr b

instance asEdgeProfileAR :: AsEdgeProfile (AudioUnitRef ptr) (SingleEdge ptr) where
  getPointers (AudioUnitRef i) = PtrArr [ i ]

instance asEdgeProfileUnit :: AsEdgeProfile Unit e where
  getPointers _ = PtrArr []

instance asEdgeProfileTupl :: EdgeListable x y => AsEdgeProfile (Tuple (AudioUnitRef ptr) x) (ManyEdges ptr y) where
  getPointers (Tuple (AudioUnitRef i) el) = let PtrArr o = getPointers' el in PtrArr ([ i ] <> o)

class Create (a :: Type) (env :: Type) (acc :: Type) (i :: Universe) (o :: Universe) (x :: Type) | a env i -> o x where
  create :: a -> Scene env acc i o x

creationStep ::
  forall env acc g.
  CreationInstructions env acc g =>
  g ->
  AudioState env acc Int
creationStep g = do
  { currentIdx, env, acc } <- get
  let
    renderable /\ internal = creationInstructions currentIdx env acc g
  modify_
    ( \i ->
        i
          { currentIdx = currentIdx + 1
          , internalGraph = M.insert currentIdx internal i.internalGraph
          , instructions = i.instructions <> renderable
          }
    )
  pure currentIdx

createAndConnect ::
  forall env acc g c i o innerTerm eprof.
  GetARefFunction g c =>
  AsEdgeProfile innerTerm eprof =>
  CreationInstructions env acc g =>
  Create c env acc i o innerTerm =>
  Proxy innerTerm ->
  g ->
  Scene env acc i o Int
createAndConnect _ g =
  Scene
    $ do
        idx <- cs
        oc <- mc
        let
          PtrArr o = getPointers oc
        modify_
          ( \i ->
              i
                { instructions =
                  i.instructions
                    <> map (flip ConnectXToY idx) o
                }
          )
        pure idx
  where
  cs = creationStep g

  (Scene mc) = (create :: c -> Scene env acc i o innerTerm) ((getARefFunction g) ARef)

instance createSinOsc ::
  ( InitialVal env acc a
  , Nat ptr
  , Succ ptr next
  ) =>
  Create
    (SinOsc a)
    env
    acc
    (UniverseC ptr (GraphC head tail) destroyed acc)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr Changing) NoEdge) (NodeListCons head tail))
        destroyed
        acc
    )
    (AudioUnitRef ptr) where
  create = Scene <<< map AudioUnitRef <<< creationStep

instance createHighpass ::
  ( InitialVal env acc a
  , InitialVal env acc b
  , ToARefFunction fc c
  , Nat ptr
  , Succ ptr next
  , Create
      c
      env
      acc
      -- we increase the pointer by 1 in this universe
      -- as the highpass consumed ptr already
      (UniverseC next graphi destroyed acc)
      (UniverseC outptr grapho destroyed acc)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Highpass a b fc)
    env
    acc
    (UniverseC ptr graphi destroyed acc)
    ( UniverseC
        outptr
        (GraphC (NodeC (THighpass ptr Changing Changing) (SingleEdge op)) nodeList)
        destroyed
        acc
    )
    (AudioUnitRef ptr) where
  create =
    Scene <<< map AudioUnitRef <<< unScene
      <<< ( createAndConnect ::
            Proxy term ->
            (Highpass a b fc) ->
            Scene env acc
              (UniverseC next graphi destroyed acc)
              (UniverseC outptr grapho destroyed acc)
              Int
        )
          (Proxy :: _ term)

instance createGain ::
  ( InitialVal env acc a
  , ToARefFunction fb b
  , Nat ptr
  , Succ ptr next
  , Create
      b
      env
      acc
      -- we increase the pointer by 1 in this universe
      -- as the gain consumed ptr already
      (UniverseC next graphi destroyed acc)
      (UniverseC outptr grapho destroyed acc)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Gain a fb)
    env
    acc
    (UniverseC ptr graphi destroyed acc)
    ( UniverseC
        outptr
        (GraphC (NodeC (TGain ptr Changing) eprof) nodeList)
        destroyed
        acc
    )
    (AudioUnitRef ptr) where
  create =
    Scene <<< map AudioUnitRef <<< unScene
      <<< ( createAndConnect ::
            Proxy term ->
            (Gain a fb) ->
            Scene env acc
              (UniverseC next graphi destroyed acc)
              (UniverseC outptr grapho destroyed acc)
              Int
        )
          (Proxy :: _ term)

instance createSpeaker ::
  ( ToARefFunction fa a
  , Nat ptr
  , Succ ptr next
  , Create
      a
      env
      acc
      -- we increase the pointer by 1 in this universe
      -- as the gain consumed ptr already
      (UniverseC next graphi destroyed acc)
      (UniverseC outptr grapho destroyed acc)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Speaker fa)
    env
    acc
    (UniverseC ptr graphi destroyed acc)
    ( UniverseC
        outptr
        (GraphC (NodeC (TSpeaker ptr) eprof) nodeList)
        destroyed
        acc
    )
    (AudioUnitRef ptr) where
  create =
    Scene <<< map AudioUnitRef <<< unScene
      <<< ( createAndConnect ::
            Proxy term ->
            (Speaker fa) ->
            Scene env acc
              (UniverseC next graphi destroyed acc)
              (UniverseC outptr grapho destroyed acc)
              Int
        )
          (Proxy :: _ term)

change' ::
  forall a g t u p i o env acc.
  GetGraph i g =>
  UniqueTerminus g t =>
  GetAudioUnit t u =>
  GetPointer u p =>
  Change p a env acc i o =>
  a -> Scene env acc i o Unit
change' = change (Proxy :: _ p)

class Change p (a :: Type) (env :: Type) (acc :: Type) (i :: Universe) (o :: Universe) | p a env i -> o where
  change :: Proxy p -> a -> Scene env acc i o Unit

--
class ModifyRes (tag :: Type) (p :: Ptr) (i :: Node) (o :: Node) (mod :: NodeList) (plist :: EdgeProfile) | tag p i -> i mod plist

instance modifyResSinOsc :: ModifyRes (SinOsc a) p (NodeC (TSinOsc p Static) e) (NodeC (TSinOsc p Changing) e) (NodeListCons (NodeC (TSinOsc p Changing) e) NodeListNil) e
else instance modifyResHighpass :: ModifyRes (SinOsc a) p (NodeC (THighpass p Static Static) e) (NodeC (THighpass p Changing Changing) e) (NodeListCons (NodeC (THighpass p Changing Changing) e) NodeListNil) e
else instance modifyResMiss :: ModifyRes tag p n n NodeListNil NoEdge

class Modify' (tag :: Type) (p :: Ptr) (i :: NodeList) (o :: NodeList) (mod :: NodeList) (nextP :: EdgeProfile) | tag p i -> o mod nextP

instance modifyNil :: Modify' tag p NodeListNil NodeListNil NodeListNil NoEdge

instance modifyCons ::
  ( ModifyRes tag p head headRes headResAsList headPlist
  , Modify' tag p tail tailRes tailResAsList tailPlist
  , NodeListAppend headResAsList tailResAsList o
  , EdgeProfileChooseGreater headPlist tailPlist plist
  ) =>
  Modify' tag p (NodeListCons head tail) (NodeListCons headRes tailRes) o plist

class Modify (tag :: Type) (p :: Ptr) (i :: Universe) (o :: Universe) (nextP :: EdgeProfile) | tag p i -> o nextP

instance modify :: (GraphToNodeList ig il, Modify' tag p il ol mod nextPL, AssertSingleton mod x, GraphToNodeList og ol) => Modify tag p (UniverseC i ig d acc) (UniverseC i og d acc) nextP

instance changeNothing ::
  Change p ARef env acc inuniv outuniv where
  change _ _ = Scene (pure unit)

instance changeSinOsc ::
  ( GetAccumulator inuniv acc
  , SetterAsChanged env acc a delta
  , IsChanging delta
  , Nat p
  , Modify (SinOsc a) p inuniv outuniv nextP
  ) =>
  Change p (SinOsc a) env acc inuniv outuniv where
  change _ (SinOsc a) =
    Scene
      $ case isChanging (Proxy :: _ delta) of
          false -> pure unit
          true -> do
            { env, acc } <- get
            let
              accAsAcc = acc

              sv = (setterVal :: a -> (env -> acc -> AudioParameter -> AudioParameter)) a

              ptr = toInt' (Proxy :: _ p)
            sosc <- M.lookup ptr <$> gets _.internalGraph
            case sosc of
              Just v -> case v of
                ASinOsc param ->
                  let
                    iv' = sv env accAsAcc param

                    AudioParameter iv = iv'
                  in
                    modify_
                      ( \i ->
                          i
                            { internalGraph = M.insert ptr (ASinOsc iv') i.internalGraph
                            , instructions =
                              i.instructions
                                <> [ SetFrequency ptr iv.param iv.timeOffset iv.transition ]
                            }
                      )
                -- bad, means there is an inconsistent state
                _ -> pure unit
              -- bad, means there is an inconsistent state
              Nothing -> pure unit

instance changeHighpass ::
  ( GetAccumulator inuniv acc
  , SetterAsChanged env acc a deltaA
  , SetterAsChanged env acc b deltaB
  , IsChanging delta
  , Nat p
  , Modify (Highpass a b c) p inuniv middle nextP
  , Change nextP c env acc middle outuniv
  ) =>
  Change p (Highpass a b c) env acc inuniv outuniv where
  change _ (Highpass a b c) =
    Scene
      $ case isChanging (Proxy :: _ delta) of
          false -> pure unit
          true -> do
            { env, acc } <- get
            let
              accAsAcc = acc

              asv = (setterVal :: a -> (env -> acc -> AudioParameter -> AudioParameter)) a

              bsv = (setterVal :: b -> (env -> acc -> AudioParameter -> AudioParameter)) b

              ptr = toInt' (Proxy :: _ p)
            sosc <- M.lookup ptr <$> gets _.internalGraph
            case sosc of
              Just v -> case v of
                AHighpass aparam bparam ->
                  let
                    aiv' = asv env accAsAcc aparam

                    AudioParameter aiv = aiv'

                    biv' = bsv env accAsAcc bparam

                    AudioParameter biv = biv'
                  in
                    modify_
                      ( \i ->
                          i
                            { internalGraph = M.insert ptr (AHighpass aiv' biv') i.internalGraph
                            , instructions =
                              i.instructions
                                <> [ SetFrequency ptr aiv.param aiv.timeOffset aiv.transition
                                  , SetQ ptr biv.param biv.timeOffset biv.transition
                                  ]
                            }
                      )
                -- bad, means there is an inconsistent state
                _ -> pure unit
              -- bad, means there is an inconsistent state
              Nothing -> pure unit

{-
derive newtype instance functorScene :: Functor m => Functor (SceneT ig og m)

derive newtype instance applyScene :: Monad m => Apply (SceneT ig og m)

derive newtype instance applicativeScene :: Monad m => Applicative (SceneT ig og m)

derive newtype instance bindScene :: Monad m => Bind (SceneT ig og m)

derive newtype instance monadScene :: Monad m => Monad (SceneT ig og m)

derive newtype instance monadTransScene :: MonadTrans (SceneT ig og)

derive newtype instance altScene :: (Monad m, Alt m) => Alt (SceneT ig og m)

derive newtype instance plusScene :: (Monad m, Plus m) => Plus (SceneT ig og m)

derive newtype instance alternativeScene :: (Monad m, Alternative m) => Alternative (SceneT ig og m)

derive newtype instance monadRecScene :: (Monad m, MonadRec m) => MonadRec (SceneT ig og m)

derive newtype instance monadZeroScene :: (Monad m, MonadZero m) => MonadZero (SceneT ig og m)

derive newtype instance monadPlusScene :: (Monad m, MonadPlus m) => MonadPlus (SceneT ig og m)

derive newtype instance lazyScene :: Lazy (SceneT ig og m a)

derive newtype instance monadEffectScene :: (Monad m, MonadEffect m) => MonadEffect (SceneT ig og m)

derive newtype instance monadContScene :: (Monad m, MonadCont m) => MonadCont (SceneT ig og m)

derive newtype instance monadThrowScene :: (Monad m, MonadThrow e m) => MonadThrow e (SceneT ig og m)

derive newtype instance monadErrorScene :: (Monad m, MonadError e m) => MonadError e (SceneT ig og m)

derive newtype instance monadAskScene :: (Monad m, MonadAsk r m) => MonadAsk r (SceneT ig og m)

derive newtype instance monadReaderScene :: (Monad m, MonadReader r m) => MonadReader r (SceneT ig og m)

derive newtype instance monadTellScene :: (Monad m, MonadTell w m) => MonadTell w (SceneT ig og m)

derive newtype instance monadWriterScene :: (Monad m, MonadWriter w m) => MonadWriter w (SceneT ig og m)

derive newtype instance semigroupScene :: (Monad m, Semigroup a) => Semigroup (SceneT ig og m a)

derive newtype instance monoidScene :: (Monad m, Monoid a) => Monoid (SceneT ig og m a)

defaultParam :: AudioParameter'
defaultParam = { param: 0.0, timeOffset: 0.0, transition: LinearRamp, forceSet: false }

data AudioParameterTransition
  = NoRamp
  | LinearRamp
  | ExponentialRamp
  | Immediately

type AudioParameter'
  = { param :: Number
    , timeOffset :: Number
    , transition :: AudioParameterTransition
    , forceSet :: Boolean
    }

newtype AudioParameter
  = AudioParameter AudioParameter'

type Scene
  = SceneT Identity

newtype SinOsc
  = SinOsc Int

newtype Highpass (nChans :: Type)
  = Highpass { nChans :: nChans, idx :: Int }

data AudioUnit_
  = SinOsc_ { idx :: Int, freq :: AudioParameter }
  | Highpass_ { idx :: Int, freq :: AudioParameter, q :: AudioParameter, a :: Int }

simpleBulder :: forall a. (Int -> a) -> (Int -> AudioUnit_) -> (Scene a)
simpleBulder ia iau =
  Scene
    $ do
        idx <- gets _.currentIdx
        modify_ (\i -> i { currentIdx = idx + 1, graph = insert idx (iau idx) i.graph })
        pure $ ia idx

type AudioInfo ch
  = { nChans :: ch, idx :: Int }

class AudioUnit a ch | a -> ch where
  audioInfo :: a -> AudioInfo ch

class Modify k v x g where
  modify :: k -> (g -> x -> v -> v) -> Scene Unit

newtype Freq
  = Freq Number

newtype Q
  = Q Number

class CSinOsc freq where
  sinOsc :: { freq :: freq } -> Scene SinOsc

instance cSinOscNumber :: CSinOsc Number where
  sinOsc { freq } = simpleBulder SinOsc (\idx -> SinOsc_ { freq: AudioParameter $ defaultParam { param = freq }, idx })

instance cSinOscAudioParameter :: CSinOsc AudioParameter where
  sinOsc { freq } = simpleBulder SinOsc (\idx -> SinOsc_ { freq, idx })

instance audioUnitSinOsc :: AudioUnit SinOsc D0 where
  audioInfo a = { nChans: d0, idx: coerce a }

class CHighpass freq q where
  highpass :: forall a ch. AudioUnit a ch => { freq :: freq, q :: q, a :: a } -> Scene (Highpass ch)

instance cHighPassFreqNumberQNumber :: CHighpass Number Number where
  highpass { freq, q, a } = highpass { freq: AudioParameter $ defaultParam { param = freq }, q: AudioParameter $ defaultParam { param = q }, a }

instance cHighPassFreqNumberQAudioParameter :: CHighpass Number AudioParameter where
  highpass { freq, q, a } = highpass { freq: AudioParameter $ defaultParam { param = freq }, q, a }

instance cHighPassFreqAudioParameterQNumber :: CHighpass AudioParameter Number where
  highpass { freq, q, a } = highpass { freq, q: AudioParameter $ defaultParam { param = q }, a }

instance cHighPassFreqAudioParamQAudioParam :: CHighpass AudioParameter AudioParameter where
  highpass { freq, q, a } = let ai = audioInfo a in simpleBulder (\idx -> Highpass { idx, nChans: ai.nChans }) (\idx -> Highpass_ { freq, q, a: ai.idx, idx })

instance audioUnitHighPass :: AudioUnit a ch => AudioUnit (Highpass ch) ch where
  audioInfo (Highpass a) = { idx: a.idx, nChans: unsafeCoerce a.nChans }
-}
{-
do
  so <- sinOsc {freq: 440.0}
  hp <  highpass {freq: 440.0, q: 1.0 } so
  -- here, the modifcation will persist everywhere
  -- so really, sinOscs don't need to hold their values at all
  -- nothing does
  -- it can all be done in the monad
  -- the only thing they need to hold is their index in the monad so that
  -- they can update the object
  modify so 440.0
-}
