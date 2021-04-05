module WAGS where

import Prelude
import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, gets, modify_, put, runState)
import Data.Either (Either(..))
import Data.Functor.Indexed (class IxFunctor, imap)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Bool (False, True)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Bin (class BinEq, class BinSub, class BinSucc, class BinToInt, BinL, D0, Ptr, PtrList, PtrListCons, PtrListNil, toInt')
import WAGS.Universe (AudioUnit, Graph, NoEdge, SingleEdge, ManyEdges, GraphC, AudioUnitList, AudioUnitCons,AudioUnitNil, Node, SkolemPairC, SkolemListNil, SkolemListCons, SkolemList, InitialGraph, NodeC, TGain, NodeList, NodeListCons, NodeListNil, THighpass, EdgeProfile, TSinOsc, TSpeaker, Universe, UniverseC)
import WAGS.Util (class Gate)

---------------------------
------------ util
cunit :: forall a. a -> Unit
cunit = const unit

class GetAccumulator (u :: Universe) (acc :: Type) | u -> acc

instance getAccumulator :: GetAccumulator (UniverseC ptr graph skolems) acc

class GetGraph (u :: Universe) (g :: Graph) | u -> g

instance getGraphUniverseC :: GetGraph (UniverseC ptr graph skolems) graph

class GetPointer (audioUnit :: AudioUnit) (ptr :: Ptr) | audioUnit -> ptr

instance getPointerSinOsc :: GetPointer (TSinOsc ptr) ptr

instance getPointerHighpass :: GetPointer (THighpass ptr) ptr

instance getPointerGain :: GetPointer (TGain ptr) ptr

instance getPointerSpeaker :: GetPointer (TSpeaker ptr) ptr

class GraphToNodeList (graph :: Graph) (nodeList :: NodeList) | graph -> nodeList, nodeList -> graph

instance graphToNodeList :: GraphToNodeList (GraphC node nodeList) (NodeListCons node nodeList)

instance graphToNodeListIG :: GraphToNodeList InitialGraph NodeListNil

class GetAudioUnit (node :: Node) (au :: AudioUnit) | node -> au

instance getAudioUnitNodeC :: GetAudioUnit (NodeC au ep) au

class LookupSkolem' (accumulator :: PtrList) (skolem :: Type) (skolemList :: SkolemList) (ptr :: PtrList) | accumulator skolem skolemList -> ptr

instance lookupSkolemNil :: LookupSkolem' accumulator ptr SkolemListNil accumulator

instance lookupSkolemCons ::
  ( TypeEqualTF skolem candidate tf
  , Gate tf (PtrListCons ptr PtrListNil) PtrListNil toComp
  , PtrListKeepSingleton toComp accumulator acc
  , LookupSkolem' acc skolem tail o
  ) =>
  LookupSkolem' accumulator skolem (SkolemListCons (SkolemPairC candidate ptr) tail) o

class LookupSkolem (skolem :: Type) (skolemList :: SkolemList) (ptr :: Ptr) | skolem skolemList -> ptr

instance lookupSkolem :: (LookupSkolem' PtrListNil skolem skolemList (PtrListCons ptr PtrListNil)) => LookupSkolem skolem skolemList ptr

class TypeEqualTF (a :: Type) (b :: Type) (c :: Type) | a b -> c

instance typeEqualTFT :: TypeEqualTF a a True
else instance typeEqualTFF :: TypeEqualTF a b False

instance skolemNotYetPresentNil :: SkolemNotYetPresent skolem SkolemListNil

instance skolemNotYetPresentCons ::
  ( TypeEqualTF skolem candidate False
  , SkolemNotYetPresentOrDiscardable skolem tail
  ) =>
  SkolemNotYetPresent skolem (SkolemListCons (SkolemPairC candidate ptr) tail)

class SkolemNotYetPresent (skolem :: Type) (skolemList :: SkolemList)

class SkolemNotYetPresentOrDiscardable (skolem :: Type) (skolemList :: SkolemList)

instance skolemNotYetPresentOrDiscardableD :: SkolemNotYetPresentOrDiscardable DiscardableSkolem skolemList
else instance skolemNotYetPresentOrDiscardableO :: SkolemNotYetPresent o skolemList => SkolemNotYetPresentOrDiscardable o skolemList

class MakeInternalSkolemStack (skolem :: Type) (ptr :: Ptr) (skolems :: SkolemList) (skolemsInternal :: SkolemList) | skolem ptr skolems -> skolemsInternal

instance makeInternalSkolemStackDiscardable :: MakeInternalSkolemStack DiscardableSkolem ptr skolems skolems
else instance makeInternalSkolemStack :: MakeInternalSkolemStack skolem ptr skolems (SkolemListCons (SkolemPairC skolem ptr) skolems)

class AudioUnitEq (a :: AudioUnit) (b :: AudioUnit) (tf :: Type) | a b -> tf

instance audioUnitEqTSinOsc :: AudioUnitEq (TSinOsc idx) (TSinOsc idx) True
else instance audioUnitEqTHighpass :: AudioUnitEq (THighpass idx) (THighpass idx) True
else instance audioUnitEqTGain :: AudioUnitEq (TGain idx) (TGain idx) True
else instance audioUnitEqTSpeaker :: AudioUnitEq (TSpeaker idx) (TSpeaker idx) True
else instance audioUnitEqFalse :: AudioUnitEq a b False

class TermToInitialAudioUnit (a :: Type) (p :: Ptr) (b :: AudioUnit) | a p -> b

instance termToInitialAudioUnitSinOsc :: TermToInitialAudioUnit (SinOsc a) ptr (TSinOsc ptr)

instance termToInitialAudioUnitHighpass :: TermToInitialAudioUnit (Highpass a b c) ptr (THighpass ptr)

instance termToInitialAudioUnitGain :: TermToInitialAudioUnit (Gain a b) ptr (TGain ptr)

instance termToInitialAudioUnitSpeaker :: TermToInitialAudioUnit (Speaker a) ptr (TSpeaker ptr)

class CreationInstructions (g :: Type) where
  creationInstructions :: Int -> g -> Array Instruction /\ AnAudioUnit

instance creationInstructionsSinOsc :: InitialVal a => CreationInstructions (SinOsc a) where
  creationInstructions idx (SinOsc a) =
    let
      iv' = initialVal a

      AudioParameter iv = iv'
    in
      [ NewUnit idx "sinosc"
      , SetFrequency idx iv.param iv.timeOffset iv.transition
      ]
        /\ ASinOsc iv'

instance creationInstructionsHighpass :: (InitialVal a, InitialVal b) => CreationInstructions (Highpass a b c) where
  creationInstructions idx (Highpass a b _) =
    let
      aiv' = initialVal a

      biv' = initialVal b

      AudioParameter aiv = aiv'

      AudioParameter biv = biv'
    in
      [ NewUnit idx "highpass"
      , SetFrequency idx aiv.param aiv.timeOffset aiv.transition
      , SetQ idx biv.param biv.timeOffset biv.transition
      ]
        /\ AHighpass aiv' biv'

instance creationInstructionsGain :: InitialVal a => CreationInstructions (Gain a b) where
  creationInstructions idx (Gain a _) =
    let
      iv' = initialVal a

      AudioParameter iv = iv'
    in
      [ NewUnit idx "gain"
      , SetGain idx iv.param iv.timeOffset iv.transition
      ]
        /\ AGain iv'

instance creationInstructionsSpeaker :: CreationInstructions (Speaker a) where
  creationInstructions idx (Speaker _) = [] /\ ASpeaker

class ChangeInstructions (g :: Type) where
  changeInstructions :: Int -> g -> AnAudioUnit -> Maybe (Array Instruction /\ AnAudioUnit)

instance changeInstructionsSinOsc :: SetterVal a => ChangeInstructions (SinOsc a) where
  changeInstructions idx (SinOsc a) = case _ of
    ASinOsc prm@(AudioParameter prm') ->
      Just $ setterVal a
        # \f ->
            let
              iv' = f prm

              AudioParameter iv = iv'
            in
              (if iv.param == prm'.param then [] else [ SetFrequency idx iv.param iv.timeOffset iv.transition ]) /\ ASinOsc iv'
    _ -> Nothing

instance changeInstructionsHighpass :: (SetterVal a, SetterVal b) => ChangeInstructions (Highpass a b c) where
  changeInstructions idx (Highpass a b _) = case _ of
    AHighpass va@(AudioParameter va') vb@(AudioParameter vb') ->
      let
        sa = setterVal a

        aiv' = sa va

        freqChanges = let AudioParameter aiv = aiv' in if aiv.param == va'.param then [] else [ SetFrequency idx aiv.param aiv.timeOffset aiv.transition ]

        sb = setterVal b

        biv' = sb vb

        qChanges = let AudioParameter biv = biv' in if biv.param == vb'.param then [] else [ SetQ idx biv.param biv.timeOffset biv.transition ]
      in
        Just
          $ (freqChanges <> qChanges)
          /\ AHighpass aiv' biv'
    _ -> Nothing

instance changeInstructionsGain :: SetterVal a => ChangeInstructions (Gain a b) where
  changeInstructions idx (Gain a _) fromMap = case fromMap of
    AGain prm@(AudioParameter prm') ->
      Just $ setterVal a
        # \f ->
            let
              iv' = f prm

              AudioParameter iv = iv'
            in
              (if iv.param == prm'.param then [] else [ SetGain idx iv.param iv.timeOffset iv.transition ]) /\ AGain iv'
    _ -> Nothing

instance changeInstructionsSpeaker :: ChangeInstructions (Speaker a) where
  changeInstructions _ _ _ = Nothing

class NodeListKeepSingleton (nodeListA :: NodeList) (nodeListB :: NodeList) (nodeListC :: NodeList) | nodeListA nodeListB -> nodeListC

instance nodeListKeepSingletonNil :: NodeListKeepSingleton NodeListNil NodeListNil NodeListNil

instance nodeListKeepSingletonL :: NodeListKeepSingleton (NodeListCons a NodeListNil) NodeListNil (NodeListCons a NodeListNil)

instance nodeListKeepSingletonR :: NodeListKeepSingleton NodeListNil (NodeListCons a NodeListNil) (NodeListCons a NodeListNil)

class PtrListKeepSingleton (ptrListA :: PtrList) (ptrListB :: PtrList) (ptrListC :: PtrList) | ptrListA ptrListB -> ptrListC

instance ptrListKeepSingletonNil :: PtrListKeepSingleton PtrListNil PtrListNil PtrListNil

instance ptrListKeepSingletonL :: PtrListKeepSingleton (PtrListCons a PtrListNil) PtrListNil (PtrListCons a PtrListNil)

instance ptrListKeepSingletonR :: PtrListKeepSingleton PtrListNil (PtrListCons a PtrListNil) (PtrListCons a PtrListNil)

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

class GraphIsCoherent (graph :: Graph)

instance graphIsCoherent ::
  ( NoNodesAreDuplicated graph
  , AllEdgesPointToNodes graph
  , NoParallelEdges graph
  ) =>
  GraphIsCoherent graph

data AudioParameterTransition
  = NoRamp
  | LinearRamp
  | ExponentialRamp
  | Immediately

derive instance eqAudioParameterTransition :: Eq AudioParameterTransition

derive instance genericAudioParameterTransition :: Generic AudioParameterTransition _

instance showAudioParameterTransition :: Show AudioParameterTransition where
  show = genericShow

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

derive instance eqInstruction :: Eq Instruction

derive instance genericInstruction :: Generic Instruction _

instance showInstruction :: Show Instruction where
  show = genericShow

instance ordInstruction :: Ord Instruction where
  compare (Stop x) (Stop y) = compare x y
  compare (Stop _) _ = LT
  compare (DisconnectXFromY x _) (DisconnectXFromY y _) = compare x y
  compare (DisconnectXFromY _ _) _ = LT
  compare (Free x) (Free y) = compare x y
  compare (Free _) _ = LT
  compare _ (Stop _) = GT
  compare _ (DisconnectXFromY _ _) = GT
  compare _ (Free _) = GT
  compare (ConnectXToY x _) (ConnectXToY y _) = compare x y
  compare (ConnectXToY _ _) _ = GT
  compare (NewUnit x _) (NewUnit y _) = compare x y
  compare (NewUnit _ _) _ = GT
  compare _ (ConnectXToY _ _) = LT
  compare _ (NewUnit _ _) = LT
  compare _ _ = EQ

testCompare :: Instruction -> Instruction -> Ordering
testCompare a b = case compare a b of
  EQ -> compare (show a) (show b)
  x -> x

type AudioState'
  = { currentIdx :: Int
    , instructions :: Array Instruction
    , internalNodes :: M.Map Int (AnAudioUnit)
    , internalEdges :: M.Map Int (Set Int)
    }

type AudioState env a
  = ReaderT env (State (AudioState')) a

newtype Frame (env :: Type) (proof :: Type) (iu :: Universe) (ou :: Universe) (a :: Type)
  = Frame (AudioState env a)

data Frame0

type InitialUniverse
  = UniverseC D0 InitialGraph SkolemListNil

type InitialFrame env a
  = Frame env Frame0 InitialUniverse InitialUniverse a

foreign import data Scene :: Type -> Type

type role Scene representational

asScene :: forall env. (env -> M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array Instruction /\ (Scene env)) -> Scene env
asScene = unsafeCoerce

oneFrame :: forall env. Scene env -> env -> M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array Instruction /\ (Scene env)
oneFrame = unsafeCoerce

instance universeIsCoherent ::
  GraphIsRenderable graph =>
  UniverseIsCoherent (UniverseC ptr graph SkolemListNil)

class UniverseIsCoherent (u :: Universe)

start :: forall env. InitialFrame env Unit
start = Frame (pure unit)

makeScene ::
  forall env proofA proofB g0 g1 a.
  UniverseIsCoherent g1 =>
  Frame env proofA g0 g1 (Either (Scene env) a) ->
  (Frame env proofB g0 g1 a -> Scene env) ->
  Scene env
makeScene (Frame m) trans = asScene go
  where
  go ev =
    let
      step1 = runReaderT m ev

      Tuple outcome newState = runState step1 initialAudioState
    in
      case outcome of
        Left s -> oneFrame s ev
        Right r ->
          newState.internalNodes /\ newState.internalEdges /\ newState.instructions
            /\ ( trans
                  $ Frame do
                      put $ newState { instructions = [] }
                      pure r
              )

loop ::
  forall env proofA i u edge a.
  TerminalIdentityEdge u edge =>
  UniverseIsCoherent u =>
  (a -> Frame env proofA u u a) ->
  Frame env proofA i u a ->
  Scene env
loop fa ma = makeScene (imap Right $ ibind ma fa) (loop fa)

branch ::
  forall env proofA i u edge a.
  TerminalIdentityEdge u edge =>
  UniverseIsCoherent u =>
  Frame env proofA u u (Either (Frame env proofA i u a -> Scene env) (a -> Frame env proofA u u a)) ->
  Frame env proofA i u a ->
  Scene env
branch mch m =
  makeScene
    ( Ix.do
        r <- m
        mbe <- mch
        case mbe of
          Left l -> ipure $ Left (l m)
          Right fa -> imap Right (fa r)
    )
    (branch mch)

infixr 6 makeScene as @>

freeze ::
  forall env proof g0 g1.
  UniverseIsCoherent g1 =>
  Frame env proof g0 g1 Unit ->
  Scene env
freeze s = makeScene (imap Right s) freeze

unFrame :: forall env proof i o a. Frame env proof i o a -> AudioState env a
unFrame (Frame state) = state

initialAudioState :: AudioState'
initialAudioState =
  { currentIdx: 0
  , instructions: []
  , internalNodes: M.empty
  , internalEdges: M.empty
  }

instance frameIxFunctor :: IxFunctor (Frame env proof) where
  imap f (Frame (a)) = Frame (f <$> a)

instance frameIxApplicative :: IxApply (Frame env proof) where
  iapply (Frame (f)) (Frame (a)) = Frame ((f <*> a))

instance frameIxApply :: IxApplicative (Frame env proof) where
  ipure a = Frame $ (pure a)

-- we ignore anything trying to write over x
instance frameIxBind :: IxBind (Frame env proof) where
  ibind (Frame (monad)) function = Frame ((monad >>= (unFrame <<< function)))

instance frameIxMonad :: IxMonad (Frame env proof)

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

param :: Number -> AudioParameter
param =
  AudioParameter
    <<< defaultParam
        { param = _
        }

derive newtype instance eqAudioParameter :: Eq AudioParameter

derive newtype instance showAudioParameter :: Show AudioParameter

class InitialVal a where
  initialVal :: a -> AudioParameter

instance initialValNumber :: InitialVal Number where
  initialVal a = AudioParameter $ defaultParam { param = a }

instance initialValAudioParameter :: InitialVal AudioParameter where
  initialVal = identity

instance initialValTuple :: InitialVal a => InitialVal (Tuple a b) where
  initialVal = initialVal <<< fst

class SetterVal a where
  setterVal :: a -> AudioParameter -> AudioParameter

instance setterValNumber :: SetterVal Number where
  setterVal = const <<< AudioParameter <<< defaultParam { param = _ }

instance setterValAudioParameter :: SetterVal AudioParameter where
  setterVal = const

instance setterValTuple :: SetterVal (Tuple a (AudioParameter -> AudioParameter)) where
  setterVal = snd

instance setterValTupleN :: SetterVal (Tuple a (AudioParameter -> Number)) where
  setterVal = map param <<< snd

instance setterValFunction :: SetterVal (AudioParameter -> AudioParameter) where
  setterVal = identity

instance setterValFunctionN :: SetterVal (AudioParameter -> Number) where
  setterVal = map param

data AudioUnitRef (ptr :: Ptr)
  = AudioUnitRef Int

data SinOsc a
  = SinOsc a

data Dup a b
  = Dup a b

data AnAudioUnit
  = ASinOsc AudioParameter
  | AHighpass AudioParameter AudioParameter
  | AGain AudioParameter
  | ASpeaker

derive instance eqAnAudioUnit :: Eq AnAudioUnit

derive instance genericAnAudioUnit :: Generic AnAudioUnit _

instance showAnAudioUnit :: Show AnAudioUnit where
  show = genericShow

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

newtype PtrArr :: forall k. k -> Type
newtype PtrArr a
  = PtrArr (Array Int)

data DiscardableSkolem

class GetSkolemFromRecursiveArgument (a :: Type) (skolem :: Type) | a -> skolem

instance getSkolemFromRecursiveArgumentF :: GetSkolemFromRecursiveArgument ((Proxy skolem) -> b) skolem
else instance getSkolemFromRecursiveArgumentC :: GetSkolemFromRecursiveArgument b DiscardableSkolem

class ToSkolemizedFunction (a :: Type) (skolem :: Type) (b :: Type) | a skolem -> b where
  toSkolemizedFunction :: a -> (Proxy skolem -> b)

instance toSkolemizedFunctionFunction :: ToSkolemizedFunction (Proxy skolem -> b) skolem b where
  toSkolemizedFunction = identity
else instance toSkolemizedFunctionConst :: ToSkolemizedFunction b skolem b where
  toSkolemizedFunction = const

class GetSkolemizedFunctionFromAU (a :: Type) (skolem :: Type) (b :: Type) | a skolem -> b where
  getSkolemizedFunctionFromAU :: a -> (Proxy skolem -> b)

instance getSkolemizedFunctionFromAUHighpass :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Highpass a b i) skolem o where
  getSkolemizedFunctionFromAU (Highpass a b c) = toSkolemizedFunction c

instance getSkolemizedFunctionFromAUGain :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Gain a i) skolem o where
  getSkolemizedFunctionFromAU (Gain a b) = toSkolemizedFunction b

instance getSkolemizedFunctionFromAUSpeaker :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Speaker i) skolem o where
  getSkolemizedFunctionFromAU (Speaker a) = toSkolemizedFunction a

class AsEdgeProfile a (b :: EdgeProfile) | a -> b where
  getPointers :: a -> PtrArr b

instance asEdgeProfileAR :: AsEdgeProfile (AudioUnitRef ptr) (SingleEdge ptr) where
  getPointers (AudioUnitRef i) = PtrArr [ i ]

instance asEdgeProfileTupl :: EdgeListable x y => AsEdgeProfile (Tuple (AudioUnitRef ptr) x) (ManyEdges ptr y) where
  getPointers (Tuple (AudioUnitRef i) el) = let PtrArr o = getPointers' el in PtrArr ([ i ] <> o)

class Create (a :: Type) (i :: Universe) (o :: Universe) (x :: Type) | a i -> o x where
  create :: forall env proof. a -> Frame env proof i o x

creationStep ::
  forall env g.
  CreationInstructions g =>
  g ->
  AudioState env Int
creationStep g = do
  currentIdx <- gets _.currentIdx
  let
    renderable /\ internal = creationInstructions currentIdx g
  modify_
    ( \i ->
        i
          { currentIdx = (currentIdx + 1)
          , internalNodes = (M.insert currentIdx (internal) i.internalNodes)
          , instructions = i.instructions <> renderable
          }
    )
  pure currentIdx

type ProxyCC skolem ptr innerTerm i o
  = Proxy (skolem /\ ptr /\ innerTerm /\ i /\ o)

createAndConnect ::
  forall env proof g (ptr :: BinL) skolem c (i :: Universe) (o :: Universe) innerTerm eprof.
  GetSkolemizedFunctionFromAU g skolem c =>
  AsEdgeProfile innerTerm eprof =>
  CreationInstructions g =>
  Create c i o innerTerm =>
  Proxy (skolem /\ (Proxy ptr) /\ innerTerm /\ (Proxy i) /\ (Proxy o)) ->
  g ->
  Frame env proof i o Int
createAndConnect _ g =
  Frame
    $ do
        idx <- cs
        let
          (Frame (mc)) =
            (create :: c -> Frame env proof i o innerTerm)
              ( ((getSkolemizedFunctionFromAU :: g -> (Proxy skolem -> c)) g)
                  Proxy
              )
        oc <- mc
        let
          PtrArr o = getPointers oc
        modify_
          ( \i ->
              i
                { internalEdges =
                  (M.insertWith S.union idx (S.fromFoldable o) i.internalEdges)
                , instructions =
                  i.instructions
                    <> map (flip ConnectXToY idx) o
                }
          )
        pure idx
  where
  cs = creationStep g

data Focus a
  = Focus a

-- end of the line in tuples
instance createUnit ::
  Create Unit u u Unit where
  create = Frame <<< pure

instance createTuple ::
  (Create x u0 u1 x', Create y u1 u2 y') =>
  Create (x /\ y) u0 u2 (x' /\ y') where
  create (x /\ y) = (Frame) $ Tuple <$> x' <*> y'
    where
    Frame (x') = (create :: forall env proof. x -> Frame env proof u0 u1 x') x

    Frame (y') = (create :: forall env proof. y -> Frame env proof u1 u2 y') y

instance createIdentity :: Create x i o r => Create (Identity x) i o r where
  create (Identity x) = create x

instance createFocus :: Create x i o r => Create (Focus x) i o r where
  create (Focus x) = create x

instance createProxy ::
  ( LookupSkolem skolem skolems ptr
  , BinToInt ptr
  ) =>
  Create
    (Proxy skolem)
    (UniverseC next graph skolems)
    (UniverseC next graph skolems)
    (AudioUnitRef ptr) where
  create _ = Frame $ (pure $ AudioUnitRef $ toInt' (Proxy :: Proxy ptr))

instance createDup ::
  ( SkolemNotYetPresent skolem skolems
  , BinToInt ptr
  , Create
      a
      (UniverseC ptr graphi skolems)
      (UniverseC midptr graphm skolems)
      ignore
  , Create
      b
      (UniverseC midptr graphm (SkolemListCons (SkolemPairC skolem ptr) skolems))
      (UniverseC outptr grapho (SkolemListCons (SkolemPairC skolem ptr) skolems))
      (AudioUnitRef midptr)
  ) =>
  Create
    (Dup a (Proxy skolem -> b))
    (UniverseC ptr graphi skolems)
    (UniverseC outptr grapho skolems)
    (AudioUnitRef midptr) where
  create (Dup a f) = Frame $ x *> y
    where
    Frame x =
      ( create ::
          forall env proof.
          a ->
          Frame env proof
            (UniverseC ptr graphi skolems)
            (UniverseC midptr graphm skolems)
            ignore
      )
        a

    Frame y =
      ( create ::
          forall env proof.
          b ->
          Frame env proof
            (UniverseC midptr graphm (SkolemListCons (SkolemPairC skolem ptr) skolems))
            (UniverseC outptr grapho (SkolemListCons (SkolemPairC skolem ptr) skolems))
            (AudioUnitRef midptr)
      )
        (f (Proxy :: _ skolem))

instance createSinOsc ::
  ( InitialVal a
  , BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (SinOsc a)
    (UniverseC ptr graph skolems)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr) NoEdge) nodeList)
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep

instance createHighpass ::
  ( InitialVal a
  , InitialVal b
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      c
      (UniverseC next graphi skolemsInternal)
      (UniverseC outptr grapho skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Highpass a b fc)
    (UniverseC ptr graphi skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (THighpass ptr) (SingleEdge op)) nodeList)
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< unFrame
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi skolemsInternal)) (Proxy (UniverseC outptr grapho skolemsInternal)))

instance createGain ::
  ( InitialVal a
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      b
      (UniverseC next graphi skolemsInternal)
      (UniverseC outptr grapho skolemsInternal)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Gain a fb)
    (UniverseC ptr graphi skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (TGain ptr) eprof) nodeList)
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< unFrame
      <<< (createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi skolemsInternal)) (Proxy (UniverseC outptr grapho skolemsInternal))))

-- toSkolemizedFunction :: a -> (Proxy skolem -> b)
instance createSpeaker ::
  ( ToSkolemizedFunction a DiscardableSkolem a
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      a
      (UniverseC next graphi skolems)
      (UniverseC outptr grapho skolems)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Speaker a)
    (UniverseC ptr graphi skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (TSpeaker ptr) eprof) nodeList)
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< unFrame
      <<< (createAndConnect (Proxy :: ProxyCC DiscardableSkolem (Proxy ptr) term (Proxy (UniverseC next graphi skolems)) (Proxy (UniverseC outptr grapho skolems))))

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

change ::
  forall edge a i env proof.
  TerminalIdentityEdge i edge =>
  Change edge a i =>
  a -> Frame env proof i i Unit
change = change' (Proxy :: _ edge)

changeAt ::
  forall ptr a i env proof.
  Change (SingleEdge ptr) a i =>
  AudioUnitRef ptr -> a -> Frame env proof i i Unit
changeAt _ = change' (Proxy :: _ (SingleEdge ptr))

class Change (p :: EdgeProfile) (a :: Type) (o :: Universe) where
  change' :: forall env proof. Proxy p -> a -> Frame env proof o o Unit

class ModifyRes (tag :: Type) (p :: Ptr) (i :: Node) (mod :: NodeList) (plist :: EdgeProfile) | tag p i -> mod plist

instance modifyResSinOsc :: ModifyRes (SinOsc a) p (NodeC (TSinOsc p) e) (NodeListCons (NodeC (TSinOsc p) e) NodeListNil) e
else instance modifyResHighpass :: ModifyRes (Highpass a b c) p (NodeC (THighpass p) e) (NodeListCons (NodeC (THighpass p) e) NodeListNil) e
else instance modifyResGain :: ModifyRes (Gain a b) p (NodeC (TGain p) e) (NodeListCons (NodeC (TGain p) e) NodeListNil) e
else instance modifyResSpeaker :: ModifyRes (Speaker a) p (NodeC (TSpeaker p) e) (NodeListCons (NodeC (TSpeaker p) e) NodeListNil) e
else instance modifyResMiss :: ModifyRes tag p n NodeListNil NoEdge

class Modify' (tag :: Type) (p :: Ptr) (i :: NodeList) (mod :: NodeList) (nextP :: EdgeProfile) | tag p i -> mod nextP

instance modifyNil :: Modify' tag p NodeListNil NodeListNil NoEdge

instance modifyCons ::
  ( ModifyRes tag p head headResAsList headPlist
  , Modify' tag p tail tailResAsList tailPlist
  , NodeListAppend headResAsList tailResAsList o
  , EdgeProfileChooseGreater headPlist tailPlist plist
  ) =>
  Modify' tag p (NodeListCons head tail) o plist

class Modify (tag :: Type) (p :: Ptr) (i :: Universe) (nextP :: EdgeProfile) | tag p i -> nextP

instance modify :: (GraphToNodeList ig il, Modify' tag p il mod nextP, AssertSingleton mod x) => Modify tag p (UniverseC i ig sk) nextP

changeAudioUnit ::
  forall g env proof acc (inuniv :: Universe) (p :: BinL) (nextP :: EdgeProfile) univ.
  GetAccumulator inuniv acc =>
  ChangeInstructions g =>
  BinToInt p =>
  Modify g p inuniv nextP =>
  Proxy ((Proxy p) /\ acc /\ (Proxy nextP) /\ Proxy inuniv) -> g -> Frame env proof univ inuniv Unit
changeAudioUnit _ g =
  Frame
    $ do
        let
          ptr = toInt' (Proxy :: _ p)
        anAudioUnit' <- M.lookup ptr <$> gets _.internalNodes
        case anAudioUnit' of
          Just (anAudioUnit) -> case changeInstructions ptr g anAudioUnit of
            Just (instr /\ au) ->
              modify_
                ( \i ->
                    i
                      { internalNodes = M.insert ptr (au) i.internalNodes
                      , instructions = i.instructions <> instr
                      }
                )
            Nothing -> pure unit
          Nothing -> pure unit

instance changeNoEdge ::
  Change NoEdge g inuniv where
  change' _ _ = Frame $ (pure unit)

instance changeSkolem ::
  Change (SingleEdge p) (Proxy skolem) inuniv where
  change' _ _ = Frame $ (pure unit)

instance changeIdentity :: Change (SingleEdge p) x inuniv => Change (SingleEdge p) (Identity x) inuniv where
  change' p (Identity x) = change' p x

instance changeFocus :: Change (SingleEdge p) x inuniv => Change (SingleEdge p) (Focus x) inuniv where
  change' p (Focus x) = change' p x

instance changeMany2 ::
  ( Change (SingleEdge p) x inuniv
  , Change (ManyEdges a b) y inuniv
  ) =>
  Change (ManyEdges p (PtrListCons a b)) (x /\ y) inuniv where
  change' _ (x /\ y) = Ix.do
    (change' :: forall env proof. Proxy (SingleEdge p) -> x -> Frame env proof inuniv inuniv Unit) Proxy x
    (change' :: forall env proof. Proxy (ManyEdges a b) -> y -> Frame env proof inuniv inuniv Unit) Proxy y

instance changeMany1 ::
  Change (SingleEdge p) a inuniv =>
  Change (ManyEdges p PtrListNil) (a /\ Unit) inuniv where
  change' _ (a /\ _) = (change' :: forall env proof. Proxy (SingleEdge p) -> a -> Frame env proof inuniv inuniv Unit) Proxy a

instance changeSinOsc ::
  ( GetAccumulator inuniv acc
  , SetterVal a
  , BinToInt p
  , Modify (SinOsc a) p inuniv nextP
  ) =>
  Change (SingleEdge p) (SinOsc a) inuniv where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ acc /\ (Proxy nextP) /\ Proxy inuniv))

instance changeHighpass ::
  ( GetAccumulator inuniv acc
  , SetterVal a
  , SetterVal b
  , BinToInt p
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , Modify (Highpass a b c) p inuniv nextP
  , Change nextP c inuniv
  ) =>
  Change (SingleEdge p) (Highpass a b fc) inuniv where
  change' _ (Highpass a b fc) =
    let
      c = (((toSkolemizedFunction :: fc -> (Proxy skolem -> c)) fc) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ acc /\ (Proxy nextP) /\ Proxy inuniv)) (Highpass a b c)
        (change' :: forall env proof. (Proxy nextP) -> c -> Frame env proof inuniv inuniv Unit) Proxy c

instance changeDup ::
  ( Create
      a
      (UniverseC D0 InitialGraph (SkolemListCons (SkolemPairC skolem D0) skolems))
      (UniverseC outptr grapho (SkolemListCons (SkolemPairC skolem D0) skolems))
      ignore
  , BinToInt p
  , BinToInt outptr
  , BinToInt continuation
  , BinSub p outptr continuation
  , Change (SingleEdge p) b inuniv
  , Change (SingleEdge continuation) a inuniv
  ) =>
  Change (SingleEdge p) (Dup a (Proxy skolem -> b)) inuniv where
  change' _ (Dup a f) = Ix.do
    (change' :: forall env proof. (Proxy (SingleEdge p)) -> b -> Frame env proof inuniv inuniv Unit) Proxy (f Proxy)
    (change' :: forall env proof. (Proxy (SingleEdge continuation)) -> a -> Frame env proof inuniv inuniv Unit) Proxy a

instance changeGain ::
  ( GetAccumulator inuniv acc
  , SetterVal a
  , BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , Modify (Gain a b) p inuniv nextP
  , Change nextP b inuniv
  ) =>
  Change (SingleEdge p) (Gain a fb) inuniv where
  change' _ (Gain a fb) =
    let
      b = (((toSkolemizedFunction :: fb -> (Proxy skolem -> b)) fb) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ acc /\ (Proxy nextP) /\ Proxy inuniv)) (Gain a b)
        (change' :: forall env proof. (Proxy nextP) -> b -> Frame env proof inuniv inuniv Unit) Proxy b

instance changeSpeaker ::
  ( GetAccumulator inuniv acc
  , BinToInt p
  , GetSkolemFromRecursiveArgument fa skolem
  , ToSkolemizedFunction fa skolem a
  , Modify (Speaker a) p inuniv nextP
  , Change nextP a inuniv
  ) =>
  Change (SingleEdge p) (Speaker fa) inuniv where
  change' _ (Speaker fa) =
    let
      a = (((toSkolemizedFunction :: fa -> (Proxy skolem -> a)) fa) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ acc /\ (Proxy nextP) /\ Proxy inuniv)) (Speaker a)
        (change' :: forall env proof. (Proxy nextP) -> a -> Frame env proof inuniv inuniv Unit) Proxy a

--------------------- getters
cursor ::
  forall edge a i env proof p.
  TerminalIdentityEdge i edge =>
  Cursor edge a i p =>
  BinToInt p =>
  a -> Frame env proof i i (AudioUnitRef p)
cursor = cursor' (Proxy :: _ edge)

class Cursor (p :: EdgeProfile) (a :: Type) (o :: Universe) (ptr :: Ptr) | p a o -> ptr where
  cursor' :: forall env proof. Proxy p -> a -> Frame env proof o o (AudioUnitRef ptr)

instance cursorRecurse :: (BinToInt head, CursorI p a o (PtrListCons head PtrListNil)) => Cursor p a o head where
  cursor' _ _ = Frame $ (pure $ AudioUnitRef (toInt' (Proxy :: Proxy head)))

class CursorRes (tag :: Type) (p :: Ptr) (i :: Node) (plist :: EdgeProfile) | tag p i -> plist

instance cursorResSinOsc :: CursorRes (SinOsc a) p (NodeC (TSinOsc p) e) e
else instance cursorResHighpass :: CursorRes (Highpass a b c) p (NodeC (THighpass p) e) e
else instance cursorResGain :: CursorRes (Gain a b) p (NodeC (TGain p) e) e
else instance cursorResSpeaker :: CursorRes (Speaker a) p (NodeC (TSpeaker p) e) e
else instance cursorResMiss :: CursorRes tag p n NoEdge

class Cursor' (tag :: Type) (p :: Ptr) (i :: NodeList) (nextP :: EdgeProfile) | tag p i -> nextP

instance cursorNil :: Cursor' tag p NodeListNil NoEdge

instance cursorCons ::
  ( CursorRes tag p head headPlist
  , Cursor' tag p tail tailPlist
  , EdgeProfileChooseGreater headPlist tailPlist plist
  ) =>
  Cursor' tag p (NodeListCons head tail) plist

class CursorX (tag :: Type) (p :: Ptr) (i :: Universe) (nextP :: EdgeProfile) | tag p i -> nextP

instance cursorX :: (GraphToNodeList ig il, Cursor' tag p il nextP) => CursorX tag p (UniverseC i ig sk) nextP

class CursorI (p :: EdgeProfile) (a :: Type) (o :: Universe) (ptr :: PtrList) | p a o -> ptr

instance cursorNoEdge :: CursorI NoEdge g inuniv PtrListNil

instance cursorSkolem :: BinToInt p => CursorI (SingleEdge p) (Proxy skolem) inuniv PtrListNil

instance cursorIdentity :: (BinToInt p, CursorI (SingleEdge p) x inuniv o) => CursorI (SingleEdge p) (Identity x) inuniv o

instance cursorFocus :: (BinToInt p, CursorI (SingleEdge p) x inuniv o) => CursorI (SingleEdge p) (Focus x) inuniv (PtrListCons p o)

instance cursorMany2 ::
  ( BinToInt p
  , BinToInt a
  , CursorI (SingleEdge p) x inuniv o0
  , CursorI (ManyEdges a b) y inuniv o1
  , PtrListAppend o0 o1 oo
  ) =>
  CursorI (ManyEdges p (PtrListCons a b)) (x /\ y) inuniv oo

instance cursorMany1 ::
  (BinToInt p, CursorI (SingleEdge p) a inuniv o) =>
  CursorI (ManyEdges p PtrListNil) (a /\ Unit) inuniv o

-- incoming to the change will be the ptr of the inner closure, which is the actual connection -- we run the inner closure to get the ptr for the outer closure
instance cursorDup ::
  ( Create
      a
      (UniverseC D0 InitialGraph (SkolemListCons (SkolemPairC skolem D0) skolems))
      (UniverseC outptr grapho (SkolemListCons (SkolemPairC skolem D0) skolems))
      ignore
  , BinToInt p
  , BinToInt outptr
  , BinToInt continuation
  , BinSub p outptr continuation
  , CursorI (SingleEdge p) b inuniv o0
  , CursorI (SingleEdge continuation) a inuniv o1
  , PtrListAppend o0 o1 oo
  ) =>
  CursorI (SingleEdge p) (Dup a (Proxy skolem -> b)) inuniv oo

instance cursorSinOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (SinOsc a) inuniv PtrListNil

instance cursorHighpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , CursorX (Highpass a b c) p inuniv nextP
  , CursorI nextP c inuniv o
  ) =>
  CursorI (SingleEdge p) (Highpass a b fc) inuniv o

instance cursorGain ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , CursorX (Gain a b) p inuniv nextP
  , CursorI nextP b inuniv o
  ) =>
  CursorI (SingleEdge p) (Gain a fb) inuniv o

instance cursorSpeaker ::
  ( BinToInt p
  , CursorX (Speaker a) p inuniv nextP
  , CursorI nextP a inuniv o
  ) =>
  CursorI (SingleEdge p) (Speaker a) inuniv o

-------------------
-- connect
class AddPointerToNode (from :: Ptr) (to :: Ptr) (i :: Node) (o :: Node) | from to i -> o

instance addPointerToNodeHPFHitSE :: AddPointerToNode from to (NodeC (THighpass to) (SingleEdge e)) (NodeC (THighpass to) (SingleEdge from))
else instance addPointerToNodeGainHitSE :: AddPointerToNode from to (NodeC (TGain to) (SingleEdge e)) (NodeC (TGain to) (ManyEdges from (PtrListCons e PtrListNil)))
else instance addPointerToNodeGainHitME :: AddPointerToNode from to (NodeC (TGain to) (ManyEdges e l)) (NodeC (TGain to) (ManyEdges from (PtrListCons e l)))
else instance addPointerToNodeSpeakerHitSE :: AddPointerToNode from to (NodeC (TSpeaker to) (SingleEdge e)) (NodeC (TSpeaker to) (ManyEdges from (PtrListCons e PtrListNil)))
else instance addPointerToNodeSpeakerHitME :: AddPointerToNode from to (NodeC (TSpeaker to) (ManyEdges e l)) (NodeC (TSpeaker to) (ManyEdges from (PtrListCons e l)))
else instance addPointerToNodeMiss :: AddPointerToNode from to i i

class AddPointerToNodes (from :: Ptr) (to :: Ptr) (i :: NodeList) (o :: NodeList) | from to i -> o

instance addPointerToNodesNil :: AddPointerToNodes a b NodeListNil NodeListNil

instance addPointerToNodesCons :: (AddPointerToNode a b head headRes, AddPointerToNodes a b tail tailRes) => AddPointerToNodes a b (NodeListCons head tail) (NodeListCons headRes tailRes)

class Connect (from :: Ptr) (to :: Ptr) (i :: Universe) (o :: Universe) | from to i -> o where
  connect :: forall env proof. AudioUnitRef from -> AudioUnitRef to -> Frame env proof i o Unit

instance connectAll :: (BinToInt from, BinToInt to, GraphToNodeList graphi nodeListI, AddPointerToNodes from to nodeListI nodeListO, GraphToNodeList grapho nodeListO) => Connect from to (UniverseC ptr graphi skolems) (UniverseC ptr grapho skolems) where
  connect (AudioUnitRef fromI) (AudioUnitRef toI) =
    Frame
      $ do
          modify_
            ( \i ->
                i
                  { internalEdges = (M.insertWith S.union toI (S.singleton fromI) i.internalEdges)
                  , instructions = i.instructions <> [ ConnectXToY fromI toI ]
                  }
            )

----------------------------------
--- disconnect
class RemovePtrFromList (ptr :: Ptr) (i :: PtrList) (o :: PtrList) | ptr i -> o

instance removePtrFromListNil :: RemovePtrFromList ptr PtrListNil PtrListNil

instance removePtrFromListCons :: (BinEq ptr head tf, RemovePtrFromList ptr tail newTail, Gate tf newTail (PtrListCons head newTail) o) => RemovePtrFromList ptr (PtrListCons head tail) o

class RemovePointerFromNode (from :: Ptr) (to :: Ptr) (i :: Node) (o :: Node) | from to i -> o

instance removePointerFromNodeHPFHitSE :: RemovePointerFromNode from to (NodeC (THighpass to) (SingleEdge from)) (NodeC (THighpass to) NoEdge)
else instance removePointerFromNodeGainHitSE :: RemovePointerFromNode from to (NodeC (TGain to) (SingleEdge from)) (NodeC (TGain to) NoEdge)
else instance removePointerFromNodeGainHitME :: (RemovePtrFromList from (PtrListCons e (PtrListCons l r)) (PtrListCons head tail)) => RemovePointerFromNode from to (NodeC (TGain to) (ManyEdges e (PtrListCons l r))) (NodeC (TGain to) (ManyEdges head tail))
else instance removePointerFromNodeSpeakerHitSE :: RemovePointerFromNode from to (NodeC (TSpeaker to) (SingleEdge from)) (NodeC (TSpeaker to) NoEdge)
else instance removePointerFromNodeSpeakerHitME :: (RemovePtrFromList from (PtrListCons e (PtrListCons l r)) (PtrListCons head tail)) => RemovePointerFromNode from to (NodeC (TSpeaker to) (ManyEdges e (PtrListCons l r))) (NodeC (TSpeaker to) (ManyEdges head tail))
else instance removePointerFromNodeMiss :: RemovePointerFromNode from to i i

class RemovePointerFromNodes (from :: Ptr) (to :: Ptr) (i :: NodeList) (o :: NodeList) | from to i -> o

instance removePointerFromNodesNil :: RemovePointerFromNodes a b NodeListNil NodeListNil

instance removePointerFromNodesCons ::
  ( RemovePointerFromNode a b head headRes
  , RemovePointerFromNodes a b tail tailRes
  ) =>
  RemovePointerFromNodes a b (NodeListCons head tail) (NodeListCons headRes tailRes)

class Disconnect (from :: Ptr) (to :: Ptr) (i :: Universe) (o :: Universe) | from to i -> o where
  disconnect :: forall env proof. AudioUnitRef from -> AudioUnitRef to -> Frame env proof i o Unit

instance disconnector ::
  ( BinToInt from
  , BinToInt to
  , GraphToNodeList graphi nodeListI
  , RemovePointerFromNodes from to nodeListI nodeListO
  , GraphToNodeList grapho nodeListO
  ) =>
  Disconnect from to (UniverseC ptr graphi skolems) (UniverseC ptr grapho skolems) where
  disconnect (AudioUnitRef fromI) (AudioUnitRef toI) =
    Frame
      $ do
          modify_
            ( \i ->
                i
                  { internalEdges = M.insertWith S.difference toI (S.singleton fromI) (i.internalEdges)
                  , instructions = i.instructions <> [ DisconnectXFromY fromI toI ]
                  }
            )

-----------------
-- destroy
class PointerNotConnected (ptr :: Ptr) (i :: Node)

instance pointerNotConnectedSinOsc :: PointerNotConnected ptr (NodeC (TSinOsc x) NoEdge)

instance pointerNotConnectedHPFNE :: PointerNotConnected ptr (NodeC (THighpass x) NoEdge)

instance pointerNotConnectedHPFSE :: BinEq ptr y False => PointerNotConnected ptr (NodeC (THighpass x) (SingleEdge y))

instance pointerNotConnectedGainNE :: PointerNotConnected ptr (NodeC (TGain x) NoEdge)

instance pointerNotConnectedGainSE :: BinEq ptr y False => PointerNotConnected ptr (NodeC (TGain x) (SingleEdge y))

instance pointerNotConnectedGainME :: PtrNotInPtrList ptr (PtrListCons y z) => PointerNotConnected ptr (NodeC (TGain x) (ManyEdges y z))

instance pointerNotConnectedSpeakerNE :: PointerNotConnected ptr (NodeC (TSpeaker x) NoEdge)

instance pointerNotConnectedSpeakerSE :: BinEq ptr y False => PointerNotConnected ptr (NodeC (TSpeaker x) (SingleEdge y))

instance pointerNotConnectedSpeakerME :: PtrNotInPtrList ptr (PtrListCons y z) => PointerNotConnected ptr (NodeC (TSpeaker x) (ManyEdges y z))

class PointerNotConnecteds (ptr :: Ptr) (i :: NodeList)

instance pointerNotConnectedsNil :: PointerNotConnecteds a NodeListNil

instance pointerNotConnectedsCons :: (PointerNotConnected a head, PointerNotConnecteds a tail) => PointerNotConnecteds a (NodeListCons head tail)

class RemovePtrFromNodeList (ptr :: Ptr) (nodeListI :: NodeList) (nodeListO :: NodeList) | ptr nodeListI -> nodeListO

instance removePtrFromNListNil :: RemovePtrFromNodeList ptr NodeListNil NodeListNil

instance removePtrFromNListCons :: (GetAudioUnit head headAu, GetPointer headAu headPtr, BinEq ptr headPtr tf, RemovePtrFromNodeList ptr tail newTail, Gate tf newTail (NodeListCons head newTail) o) => RemovePtrFromNodeList ptr (NodeListCons head tail) o

class Destroy (ptr :: Ptr) (i :: Universe) (o :: Universe) | ptr i -> o where
  destroy :: forall env proof. AudioUnitRef ptr -> Frame env proof i o Unit

instance destroyer ::
  ( BinToInt ptr
  , GraphToNodeList graphi nodeListI
  , PointerNotConnecteds ptr nodeListI
  , RemovePtrFromNodeList ptr nodeListI nodeListO
  , GraphToNodeList grapho nodeListO
  ) =>
  Destroy ptr (UniverseC x graphi skolems) (UniverseC x grapho skolems) where
  destroy (AudioUnitRef ptrI) =
    Frame
      $ do
          modify_
            ( \i ->
                i
                  { internalNodes = M.delete ptrI (i.internalNodes)
                  , internalEdges = M.delete ptrI (i.internalEdges)
                  , instructions = i.instructions <> [ Free ptrI, Stop ptrI ]
                  }
            )

-- getters
env ::
  forall env proof i.
  Frame env proof i i env
env = Frame ask
