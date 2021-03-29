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
import Control.Monad.State (class MonadTrans, State, StateT, execState, get, gets, modify_, withState)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT(..), runWriterT)
import Control.MonadPlus (class MonadPlus, class MonadZero)
import Control.Plus (class Plus)
import Data.Functor.Indexed (class IxFunctor)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Control.Monad.Indexed.Qualified as Ix
import Data.Map (Map, insert)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Bool (False, True)
import Data.Typelevel.Num (class Nat, class Succ, D0, d0, toInt')
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prim.TypeError (class Warn, Above, Beside, Quote, Text)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

foreign import data InitialGraph :: Graph

data Universe

-- currentIdx graph destroyable skolems accumulator
foreign import data UniverseC :: Ptr -> Graph -> PtrList -> SkolemList -> Type -> Universe

---------------------------
------------ util
cunit :: forall a. a -> Unit
cunit = const unit

class GetAccumulator (u :: Universe) (acc :: Type) | u -> acc

instance getAccumulator :: GetAccumulator (UniverseC ptr graph destroyable skolems acc) acc

class GetGraph (u :: Universe) (g :: Graph) | u -> g

instance getGraphUniverseC :: GetGraph (UniverseC ptr graph destroyed skolems acc) graph

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

class MakeInternalSkolemStack (skolem :: Ptr) (ptr :: Ptr) (skolems :: SkolemList) (skolemsInternal :: SkolemList) | skolem ptr skolems -> skolemsInternal

instance makeInternalSkolemStackDiscardable :: MakeInternalSkolemStack DiscardableSkolem ptr skolems skolems
else instance makeInternalSkolemStack :: MakeInternalSkolemStack skolem ptr skolems (SkolemListCons (SkolemPairC skolem ptr) skolems)

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

class KeepChanging (a :: TAudioParameter) (b :: TAudioParameter) (c :: TAudioParameter) | a b -> c

instance keepChangingSL :: KeepChanging Changing a Changing
else instance keepChangingSR :: KeepChanging b Changing Changing
else instance staticChange :: KeepChanging a b Static

class ChangeInstructions (env :: Type) (acc :: Type) (g :: Type) where
  changeInstructions :: Int -> env -> acc -> g -> AnAudioUnit -> Maybe (Array Instruction /\ AnAudioUnit)

instance changeInstructionsSinOsc :: (SetterAsChanged env acc a delta, IsChanging delta) => ChangeInstructions env acc (SinOsc a) where
  changeInstructions idx env acc (SinOsc a) = case _ of
    ASinOsc prm -> case (isChanging (Proxy :: _ delta)) of
      false -> Nothing
      true ->
        let
          -- case (isChanging)
          iv' = ((setterVal :: a -> (env -> acc -> AudioParameter -> AudioParameter)) a) env acc prm

          AudioParameter iv = iv'
        in
          Just $ [ SetFrequency idx iv.param iv.timeOffset iv.transition ] /\ ASinOsc iv'
    _ -> Nothing

instance changeInstructionsHighpass :: (SetterAsChanged env acc a delta1, SetterAsChanged env acc b delta2, IsChanging delta1, IsChanging delta2) => ChangeInstructions env acc (Highpass a b c) where
  changeInstructions idx env acc (Highpass a b _) = case _ of
    AHighpass va vb ->
      --todo: split across two functions?
      let
        aic = isChanging (Proxy :: Proxy delta1)

        aiv' = case aic of
          true -> ((setterVal :: a -> (env -> acc -> AudioParameter -> AudioParameter)) a) env acc va
          false -> va

        freqChanges = case aic of
          false -> []
          true -> let AudioParameter aiv = aiv' in [ SetFrequency idx aiv.param aiv.timeOffset aiv.transition ]

        bic = isChanging (Proxy :: Proxy delta2)

        biv' = case bic of
          true -> ((setterVal :: b -> (env -> acc -> AudioParameter -> AudioParameter)) b) env acc vb
          false -> vb

        qChanges = case aic of
          false -> []
          true -> let AudioParameter biv = biv' in [ SetQ idx biv.param biv.timeOffset biv.transition ]
      in
        Just
          $ (freqChanges <> qChanges)
          /\ AHighpass aiv' biv'
    _ -> Nothing

instance changeInstructionsGain :: (SetterAsChanged env acc a delta, IsChanging delta) => ChangeInstructions env acc (Gain a b) where
  changeInstructions idx env acc (Gain a _) fromMap = case (isChanging (Proxy :: _ delta)) of
    false -> Nothing
    true -> case fromMap of
      AGain prm ->
        let
          sv = (setterVal :: a -> (env -> acc -> AudioParameter -> AudioParameter)) a

          iv' = sv env acc prm

          AudioParameter iv = iv'
        in
          Just $ [ SetGain idx iv.param iv.timeOffset iv.transition ] /\ AGain iv'
      _ -> Nothing

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

instance allNodesAreFullyHydratedConsTGainSE :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TGain a b) (SingleEdge e)) tail)

instance allNodesAreFullyHydratedConsTGainME :: AllNodesAreFullyHydratedNL tail => AllNodesAreFullyHydratedNL (NodeListCons (NodeC (TGain a b) (ManyEdges e l)) tail)

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

-- for any given step - worth it?
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

type AudioState' env
  = { env :: env
    , acc :: Void
    , currentIdx :: Int
    , instructions :: Array Instruction
    , internalNodes :: Map Int AnAudioUnit
    , internalEdges :: Map Int (Set Int)
    }

type AudioState env a
  = WriterT (Endo Function (AudioState' env)) (State (AudioState' env)) a

newtype Frame (env :: Type) (proof :: Type) (iu :: Universe) (ou :: Universe) (a :: Type)
  = Frame (AudioState env a)

data Frame0

type InitialFrame env acc og
  = Frame env Frame0 (UniverseC D0 InitialGraph PtrListNil SkolemListNil acc) og Unit

foreign import data Scene :: Type -> Type -> Type

type role Scene representational representational

asScene :: forall env proof. (env -> Map Int AnAudioUnit /\ Map Int (Set Int) /\ Array Instruction /\ (Scene env proof)) -> Scene env proof
asScene = unsafeCoerce

oneFrame :: forall env proof. Scene env proof -> env -> Map Int AnAudioUnit /\ Map Int (Set Int) /\ Array Instruction /\ (Scene env proof)
oneFrame = unsafeCoerce

instance universeIsCoherent ::
  GraphIsRenderable graph =>
  UniverseIsCoherent (UniverseC ptr graph destroyed SkolemListNil acc) where
  assertCoherence _ = unit

class UniverseIsCoherent (u :: Universe) where
  assertCoherence :: forall env proof i x. Frame env proof i u x -> Unit

start ::
  forall env acc g0 g1.
  UniverseIsCoherent g0 =>
  AsStatic g0 g1 =>
  acc ->
  InitialFrame env acc g0 ->
  (forall proof. Frame env proof g1 g1 Unit -> Scene env proof) ->
  Scene env Frame0
start a b = makeScene0T (a /\ b)

makeScene0T ::
  forall env acc g0 g1.
  UniverseIsCoherent g0 =>
  AsStatic g0 g1 =>
  acc /\ InitialFrame env acc g0 ->
  (forall proof. Frame env proof g1 g1 Unit -> Scene env proof) ->
  Scene env Frame0
makeScene0T (acc /\ fr@(Frame f)) trans = asScene go
  where
  go env =
    let
      os =
        execState (map fst (runWriterT f))
          (initialAudioState env acc)

      scene = trans $ Frame $ WriterT (pure (Tuple unit (Endo (const $ os))))
    in
      os.internalNodes /\ os.internalEdges /\ os.instructions /\ scene

infixr 6 makeScene0T as @@>

makeScene' ::
  forall env proof g0 g1 g2.
  (Frame env proof g0 g1 Unit -> Unit) ->
  (Frame env proof g0 g1 Unit -> Frame env proof g2 g2 Unit) ->
  Frame env proof g0 g1 Unit ->
  (Frame env proof g2 g2 Unit -> Scene env proof) ->
  Scene env proof
makeScene' _ _ fr@(Frame f) trans = asScene go
  where
  go env =
    let
      rt = runWriterT f

      stateM = map fst rt

      initialSt = map snd rt

      ias = initialAudioState env (unsafeCoerce unit)

      os =
        execState
          ( do
              Endo s <- initialSt
              withState (const $ (s ias) { env = env, instructions = [] }) stateM
          )
          ias

      scene = trans $ Frame $ WriterT (pure (Tuple unit (Endo (const $ os))))
    in
      os.internalNodes /\ os.internalEdges /\ os.instructions /\ scene

makeScene ::
  forall env proof g0 g1 g2.
  UniverseIsCoherent g1 =>
  AsStatic g1 g2 =>
  Frame env proof g0 g1 Unit ->
  (Frame env proof g2 g2 Unit -> Scene env proof) ->
  Scene env proof
makeScene = makeScene' assertCoherence asStatic

infixr 6 makeScene as @>

loop' ::
  forall env proof g0 g1.
  (Frame env proof g0 g1 Unit -> Unit) ->
  Frame env proof g0 g1 Unit ->
  Scene env proof
loop' _ fr = makeScene' cunit (\(Frame s) -> Frame s) fr (loop' cunit)

loop ::
  forall env proof g0 g1.
  UniverseIsCoherent g1 =>
  Frame env proof g0 g1 Unit ->
  Scene env proof
loop fr = makeScene' cunit (\(Frame s) -> Frame s) fr (loop' cunit)

unFrame :: forall env proof i o a. Frame env proof i o a -> AudioState env a
unFrame (Frame state) = state

initialAudioState :: forall env acc. env -> acc -> AudioState' env
initialAudioState env acc =
  { env: env
  , acc: unsafeCoerce acc
  , currentIdx: 0
  , instructions: []
  , internalNodes: M.empty
  , internalEdges: M.empty
  }

instance sceneIxFunctor :: IxFunctor (Frame env proof) where
  imap f (Frame a) = Frame (f <$> a)

instance sceneIxApplicative :: IxApply (Frame env proof) where
  iapply (Frame f) (Frame a) = Frame (f <*> a)

instance sceneIxApply :: IxApplicative (Frame env proof) where
  ipure a = Frame $ pure a

instance sceneIxBind :: IxBind (Frame env proof) where
  ibind (Frame monad) function = Frame (monad >>= (unFrame <<< function))

instance sceneIxMonad :: IxMonad (Frame env proof)

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

param :: Number -> AudioParameter
param =
  AudioParameter
    <<< defaultParam
        { param = _
        }

derive newtype instance eqAudioParameter :: Eq AudioParameter

derive newtype instance showAudioParameter :: Show AudioParameter

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

instance setterValFunction :: SetterVal env acc (env -> acc -> AudioParameter -> AudioParameter) where
  setterVal = identity

class IsChanging (b :: TAudioParameter) where
  isChanging :: Proxy b -> Boolean

class SetterVal env acc a <= SetterAsChanged env acc a (b :: TAudioParameter) | a -> b

instance setterAsChangedNumber :: SetterAsChanged env acc Number Static

instance setterAsChangedAudioParameter :: SetterAsChanged env acc AudioParameter Static

instance setterAsChangedTuple :: SetterAsChanged env acc (Tuple a (env -> acc -> AudioParameter -> AudioParameter)) Changing

instance setterAsChangedFunction :: SetterAsChanged env acc (env -> acc -> AudioParameter -> AudioParameter) Changing

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

class DynToStat'' (i :: AudioUnit) (o :: AudioUnit) | i -> o

instance d2sTSinOsc :: DynToStat'' (TSinOsc idx freq) (TSinOsc idx Static)

instance d2sTHighpass :: DynToStat'' (THighpass idx freq q) (THighpass idx Static Static)

instance d2sTGain :: DynToStat'' (TGain idx vol) (TGain idx Static)

instance d2sTSpeaker :: DynToStat'' (TSpeaker idx) (TSpeaker idx)

class DynToStat' (i :: Node) (o :: Node) | i -> o

instance d2s :: DynToStat'' i o => DynToStat' (NodeC i x) (NodeC o x)

class DynToStat (i :: NodeList) (o :: NodeList) | i -> o

instance dynToStatNil :: DynToStat NodeListNil NodeListNil

instance dynToStatCons :: (DynToStat' head newHead, DynToStat tail newTail) => DynToStat (NodeListCons head tail) (NodeListCons newHead newTail)

class AsStatic (m :: Universe) (o :: Universe) | m -> o where
  asStatic :: forall env proof i x. Frame env proof i m x -> Frame env proof o o x

-- only do when there are no skolems
-- this will force the step to the end
instance asStaticAll :: (GraphToNodeList dynGraph dynGraphL, DynToStat dynGraphL statGraphL, GraphToNodeList statGraph statGraphL) => AsStatic (UniverseC ptr dynGraph destroyed SkolemListNil acc) (UniverseC ptr statGraph destroyed SkolemListNil acc) where
  asStatic (Frame a) = Frame a

class Create (a :: Type) (env :: Type) (proof :: Type) (i :: Universe) (o :: Universe) (x :: Type) | a env i -> o x where
  create :: a -> Frame env proof i o x

creationStep ::
  forall env acc g.
  CreationInstructions env acc g =>
  Proxy acc ->
  g ->
  AudioState env Int
creationStep _ g = do
  { currentIdx, env, acc } <- get
  let
    renderable /\ internal = creationInstructions currentIdx env (unsafeCoerce acc :: acc) g
  modify_
    ( \i ->
        i
          { currentIdx = currentIdx + 1
          , internalNodes = M.insert currentIdx internal i.internalNodes
          , instructions = i.instructions <> renderable
          }
    )
  pure currentIdx

type ProxyCC acc skolem ptr innerTerm
  = Proxy (acc /\ skolem /\ ptr /\ innerTerm)

createAndConnect ::
  forall env acc proof g ptr skolem c i o innerTerm eprof.
  GetSkolemizedFunctionFromAU g skolem c =>
  AsEdgeProfile innerTerm eprof =>
  CreationInstructions env acc g =>
  Create c env proof i o innerTerm =>
  Proxy (acc /\ skolem /\ ptr /\ innerTerm) ->
  g ->
  Frame env proof i o Int
createAndConnect _ g =
  Frame
    $ do
        idx <- cs
        let
          (Frame mc) =
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
                  M.insertWith S.union idx (S.fromFoldable o) i.internalEdges
                , instructions =
                  i.instructions
                    <> map (flip ConnectXToY idx) o
                }
          )
        pure idx
  where
  cs = creationStep (Proxy :: _ acc) g

-- end of the line in tuples
instance createUnit ::
  Create
    Unit
    env
    proof
    u
    u
    Unit where
  create = Frame <<< pure

instance createTuple ::
  (Create x env proof u0 u1 x', Create y env proof u1 u2 y') =>
  Create (x /\ y) env proof u0 u2 (x' /\ y') where
  create (x /\ y) = Frame $ Tuple <$> x' <*> y'
    where
    Frame x' = (create :: x -> Frame env proof u0 u1 x') x

    Frame y' = (create :: y -> Frame env proof u1 u2 y') y

instance createProxy ::
  ( LookupSkolem skolem skolems ptr
  , Nat ptr
  ) =>
  Create
    (Proxy skolem)
    env
    proof
    (UniverseC next graph destroyed skolems acc)
    (UniverseC next graph destroyed skolems acc)
    (AudioUnitRef ptr) where
  create _ = Frame (pure $ AudioUnitRef $ toInt' (Proxy :: Proxy ptr))

instance createDup ::
  ( Nat ptr
  , SkolemNotYetPresent skolem skolems
  , Create
      a
      env
      proof
      (UniverseC ptr graphi destroyed skolems acc)
      (UniverseC midptr graphm destroyed skolems acc)
      (AudioUnitRef ptr)
  , Create
      b
      env
      proof
      (UniverseC midptr graphm destroyed (SkolemListCons (SkolemPairC skolem ptr) skolems) acc)
      (UniverseC outptr grapho destroyed (SkolemListCons (SkolemPairC skolem ptr) skolems) acc)
      ignore
  ) =>
  Create
    (Dup a (Proxy skolem -> b))
    env
    proof
    (UniverseC ptr graphi destroyed skolems acc)
    (UniverseC outptr grapho destroyed skolems acc)
    (AudioUnitRef ptr) where
  create (Dup a f) = Frame $ x <* y
    where
    Frame x =
      ( create ::
          a ->
          Frame env proof
            (UniverseC ptr graphi destroyed skolems acc)
            (UniverseC midptr graphm destroyed skolems acc)
            (AudioUnitRef ptr)
      )
        a

    Frame y =
      ( create ::
          b ->
          Frame env proof
            (UniverseC midptr graphm destroyed (SkolemListCons (SkolemPairC skolem ptr) skolems) acc)
            (UniverseC outptr grapho destroyed (SkolemListCons (SkolemPairC skolem ptr) skolems) acc)
            ignore
      )
        (f (Proxy :: _ skolem))

instance createSinOsc ::
  ( InitialVal env acc a
  , Nat ptr
  , Succ ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (SinOsc a)
    env
    proof
    (UniverseC ptr graph destroyed skolems acc)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr Changing) NoEdge) nodeList)
        destroyed
        skolems
        acc
    )
    (AudioUnitRef ptr) where
  create = Frame <<< map AudioUnitRef <<< creationStep (Proxy :: _ acc)

instance createHighpass ::
  ( InitialVal env acc a
  , InitialVal env acc b
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , Nat ptr
  , Succ ptr next
  , Create
      c
      env
      proof
      (UniverseC next graphi destroyed skolemsInternal acc)
      (UniverseC outptr grapho destroyed skolemsInternal acc)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Highpass a b fc)
    env
    proof
    (UniverseC ptr graphi destroyed skolems acc)
    ( UniverseC
        outptr
        (GraphC (NodeC (THighpass ptr Changing Changing) (SingleEdge op)) nodeList)
        destroyed
        skolems
        acc
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< map AudioUnitRef <<< unFrame
      <<< ( createAndConnect ::
            ProxyCC acc skolem ptr term ->
            (Highpass a b fc) ->
            Frame env proof
              (UniverseC next graphi destroyed skolemsInternal acc)
              (UniverseC outptr grapho destroyed skolemsInternal acc)
              Int
        )
          Proxy

instance createGain ::
  ( InitialVal env acc a
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , Nat ptr
  , Succ ptr next
  , Create
      b
      env
      proof
      (UniverseC next graphi destroyed skolemsInternal acc)
      (UniverseC outptr grapho destroyed skolemsInternal acc)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Gain a fb)
    env
    proof
    (UniverseC ptr graphi destroyed skolems acc)
    ( UniverseC
        outptr
        (GraphC (NodeC (TGain ptr Changing) eprof) nodeList)
        destroyed
        skolems
        acc
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< map AudioUnitRef <<< unFrame
      <<< ( createAndConnect ::
            ProxyCC acc skolem ptr term ->
            (Gain a fb) ->
            Frame env proof
              (UniverseC next graphi destroyed skolemsInternal acc)
              (UniverseC outptr grapho destroyed skolemsInternal acc)
              Int
        )
          Proxy

-- toSkolemizedFunction :: a -> (Proxy skolem -> b)
instance createSpeaker ::
  ( ToSkolemizedFunction a DiscardableSkolem a
  , Nat ptr
  , Succ ptr next
  , Create
      a
      env
      proof
      (UniverseC next graphi destroyed skolems acc)
      (UniverseC outptr grapho destroyed skolems acc)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Speaker a)
    env
    proof
    (UniverseC ptr graphi destroyed skolems acc)
    ( UniverseC
        outptr
        (GraphC (NodeC (TSpeaker ptr) eprof) nodeList)
        destroyed
        skolems
        acc
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< map AudioUnitRef <<< unFrame
      <<< ( createAndConnect ::
            ProxyCC acc DiscardableSkolem ptr term ->
            (Speaker a) ->
            Frame env proof
              (UniverseC next graphi destroyed skolems acc)
              (UniverseC outptr grapho destroyed skolems acc)
              Int
        )
          Proxy

change ::
  forall a g t u (p :: EdgeProfile) i o env proof.
  GetGraph i g =>
  UniqueTerminus g t =>
  GetAudioUnit t u =>
  Change p a env proof i o =>
  a -> Frame env proof i o Unit
change = change' (Proxy :: _ p)

class Change (p :: EdgeProfile) (a :: Type) (env :: Type) (proof :: Type) (i :: Universe) (o :: Universe) | p a env i -> o where
  change' :: Proxy p -> a -> Frame env proof i o Unit

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

instance modify :: (GraphToNodeList ig il, Modify' tag p il ol mod nextPL, AssertSingleton mod x, GraphToNodeList og ol) => Modify tag p (UniverseC i ig d sk acc) (UniverseC i og d sk acc) nextP

instance changeSkolem ::
  Change p (Proxy skolem) env proof inuniv outuniv where
  change' _ _ = Frame (pure unit)

instance changeNothing ::
  Change p Unit env proof inuniv outuniv where
  change' _ _ = Frame (pure unit)

changeAudioUnit ::
  forall g env proof acc inuniv outuniv p nextP.
  GetAccumulator inuniv acc =>
  ChangeInstructions env acc g =>
  Nat p =>
  Modify g p inuniv outuniv nextP =>
  Proxy (p /\ acc /\ (Proxy nextP)) -> g -> Frame env proof inuniv outuniv Unit
changeAudioUnit _ g =
  Frame
    $ do
        { env, acc } <- get
        let
          ptr = toInt' (Proxy :: _ p)
        anAudioUnit' <- M.lookup ptr <$> gets _.internalNodes
        case anAudioUnit' of
          Just anAudioUnit -> case changeInstructions ptr env (unsafeCoerce acc :: acc) g anAudioUnit of
            Just (instr /\ au) ->
              modify_
                ( \i ->
                    i
                      { internalNodes = M.insert ptr au i.internalNodes
                      , instructions = i.instructions <> instr
                      }
                )
            Nothing -> pure unit
          Nothing -> pure unit

instance changeSinOsc ::
  ( GetAccumulator inuniv acc
  , SetterAsChanged env acc a delta
  , IsChanging delta
  , Nat p
  , Modify (SinOsc a) p inuniv outuniv nextP
  ) =>
  Change (SingleEdge p) (SinOsc a) env proof inuniv outuniv where
  change' _ = (changeAudioUnit :: Proxy (p /\ acc /\ (Proxy nextP)) -> (SinOsc a) -> Frame env proof inuniv outuniv Unit) Proxy

instance changeHighpass ::
  ( GetAccumulator inuniv acc
  , SetterAsChanged env acc a deltaA
  , SetterAsChanged env acc b deltaB
  , IsChanging deltaA
  , IsChanging deltaB
  , KeepChanging deltaA deltaB delta
  , Nat p
  , Modify (Highpass a b c) p inuniv middle nextP
  , Change nextP c env proof middle outuniv
  ) =>
  Change (SingleEdge p) (Highpass a b c) env proof inuniv outuniv where
  change' _ (Highpass a b c) = Ix.do
    (changeAudioUnit :: Proxy (p /\ acc /\ (Proxy nextP)) -> (Highpass a b c) -> Frame env proof inuniv middle Unit) Proxy (Highpass a b c)
    (change' :: (Proxy nextP) -> c -> Frame env proof middle outuniv Unit) Proxy c

instance changeGain ::
  ( GetAccumulator inuniv acc
  , SetterAsChanged env acc a delta
  , IsChanging delta
  , Nat p
  , Modify (Gain a b) p inuniv middle nextP
  , Change nextP b env proof middle outuniv
  ) =>
  Change (SingleEdge p) (Gain a b) env proof inuniv outuniv where
  change' _ (Gain a b) = Ix.do
    (changeAudioUnit :: Proxy (p /\ acc /\ (Proxy nextP)) -> (Gain a b) -> Frame env proof inuniv middle Unit) Proxy (Gain a b)
    (change' :: (Proxy nextP) -> b -> Frame env proof middle outuniv Unit) Proxy b

instance changeSpeaker ::
  ( Nat p
  , Change nextP a env proof inuniv outuniv
  ) =>
  Change (SingleEdge p) (Speaker a) env proof inuniv outuniv where
  change' _ (Speaker a) =  (change' :: (Proxy nextP) -> a -> Frame env proof inuniv outuniv Unit) Proxy a

{-
derive newtype instance functorFrame :: Functor m => Functor (FrameT ig og m)

derive newtype instance applyFrame :: Monad m => Apply (FrameT ig og m)

derive newtype instance applicativeFrame :: Monad m => Applicative (FrameT ig og m)

derive newtype instance bindFrame :: Monad m => Bind (FrameT ig og m)

derive newtype instance monadFrame :: Monad m => Monad (FrameT ig og m)

derive newtype instance monadTransFrame :: MonadTrans (FrameT ig og)

derive newtype instance altFrame :: (Monad m, Alt m) => Alt (FrameT ig og m)

derive newtype instance plusFrame :: (Monad m, Plus m) => Plus (FrameT ig og m)

derive newtype instance alternativeFrame :: (Monad m, Alternative m) => Alternative (FrameT ig og m)

derive newtype instance monadRecFrame :: (Monad m, MonadRec m) => MonadRec (FrameT ig og m)

derive newtype instance monadZeroFrame :: (Monad m, MonadZero m) => MonadZero (FrameT ig og m)

derive newtype instance monadPlusFrame :: (Monad m, MonadPlus m) => MonadPlus (FrameT ig og m)

derive newtype instance lazyFrame :: Lazy (FrameT ig og m a)

derive newtype instance monadEffectFrame :: (Monad m, MonadEffect m) => MonadEffect (FrameT ig og m)

derive newtype instance monadContFrame :: (Monad m, MonadCont m) => MonadCont (FrameT ig og m)

derive newtype instance monadThrowFrame :: (Monad m, MonadThrow e m) => MonadThrow e (FrameT ig og m)

derive newtype instance monadErrorFrame :: (Monad m, MonadError e m) => MonadError e (FrameT ig og m)

derive newtype instance monadAskFrame :: (Monad m, MonadAsk r m) => MonadAsk r (FrameT ig og m)

derive newtype instance monadReaderFrame :: (Monad m, MonadReader r m) => MonadReader r (FrameT ig og m)

derive newtype instance monadTellFrame :: (Monad m, MonadTell w m) => MonadTell w (FrameT ig og m)

derive newtype instance monadWriterFrame :: (Monad m, MonadWriter w m) => MonadWriter w (FrameT ig og m)

derive newtype instance semigroupFrame :: (Monad m, Semigroup a) => Semigroup (FrameT ig og m a)

derive newtype instance monoidFrame :: (Monad m, Monoid a) => Monoid (FrameT ig og m a)

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

type Frame
  = FrameT Identity

newtype SinOsc
  = SinOsc Int

newtype Highpass (nChans :: Type)
  = Highpass { nChans :: nChans, idx :: Int }

data AudioUnit_
  = SinOsc_ { idx :: Int, freq :: AudioParameter }
  | Highpass_ { idx :: Int, freq :: AudioParameter, q :: AudioParameter, a :: Int }

simpleBulder :: forall a. (Int -> a) -> (Int -> AudioUnit_) -> (Frame a)
simpleBulder ia iau =
  Frame
    $ do
        idx <- gets _.currentIdx
        modify_ (\i -> i { currentIdx = idx + 1, graph = insert idx (iau idx) i.graph })
        pure $ ia idx

type AudioInfo ch
  = { nChans :: ch, idx :: Int }

class AudioUnit a ch | a -> ch where
  audioInfo :: a -> AudioInfo ch

class Modify k v x g where
  modify :: k -> (g -> x -> v -> v) -> Frame Unit

newtype Freq
  = Freq Number

newtype Q
  = Q Number

class CSinOsc freq where
  sinOsc :: { freq :: freq } -> Frame SinOsc

instance cSinOscNumber :: CSinOsc Number where
  sinOsc { freq } = simpleBulder SinOsc (\idx -> SinOsc_ { freq: AudioParameter $ defaultParam { param = freq }, idx })

instance cSinOscAudioParameter :: CSinOsc AudioParameter where
  sinOsc { freq } = simpleBulder SinOsc (\idx -> SinOsc_ { freq, idx })

instance audioUnitSinOsc :: AudioUnit SinOsc D0 where
  audioInfo a = { nChans: d0, idx: coerce a }

class CHighpass freq q where
  highpass :: forall a ch. AudioUnit a ch => { freq :: freq, q :: q, a :: a } -> Frame (Highpass ch)

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
