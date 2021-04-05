module WAGS.Disconnect where

import Prelude

import Control.Monad.State (modify_)
import Data.Map as M
import Data.Set as S
import Data.Typelevel.Bool (class Or, False, True)
import WAGS.Control.Types (Frame(..))
import WAGS.Rendered (Instruction(..))
import WAGS.Universe.AudioUnit (AudioUnitRef(..), TGain, THighpass, TSpeaker)
import WAGS.Universe.Bin (class BinEq, class BinToInt, Ptr, PtrList, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (Universe, UniverseC)
import WAGS.Util (class Gate)

class RemovePtrFromList (ptr :: Ptr) (i :: PtrList) (o :: PtrList) | ptr i -> o

instance removePtrFromListNil :: RemovePtrFromList ptr PtrListNil PtrListNil

instance removePtrFromListCons ::
  ( BinEq ptr head tf
  , RemovePtrFromList ptr tail newTail
  , Gate tf newTail (PtrListCons head newTail) o
  ) =>
  RemovePtrFromList ptr (PtrListCons head tail) o

class RemovePointerFromNode (from :: Ptr) (to :: Ptr) (i :: Node) (o :: Node) (tf :: Type) | from to i -> o tf

instance removePointerFromNodeHPFHitSE :: RemovePointerFromNode from to (NodeC (THighpass to) (SingleEdge from)) (NodeC (THighpass to) NoEdge) True
else instance removePointerFromNodeGainHitSE :: RemovePointerFromNode from to (NodeC (TGain to) (SingleEdge from)) (NodeC (TGain to) NoEdge) True
else instance removePointerFromNodeGainHitME :: (RemovePtrFromList from (PtrListCons e (PtrListCons l r)) (PtrListCons head tail)) => RemovePointerFromNode from to (NodeC (TGain to) (ManyEdges e (PtrListCons l r))) (NodeC (TGain to) (ManyEdges head tail)) True
else instance removePointerFromNodeSpeakerHitSE :: RemovePointerFromNode from to (NodeC (TSpeaker to) (SingleEdge from)) (NodeC (TSpeaker to) NoEdge) True
else instance removePointerFromNodeSpeakerHitME :: (RemovePtrFromList from (PtrListCons e (PtrListCons l r)) (PtrListCons head tail)) => RemovePointerFromNode from to (NodeC (TSpeaker to) (ManyEdges e (PtrListCons l r))) (NodeC (TSpeaker to) (ManyEdges head tail)) True
else instance removePointerFromNodeMiss :: RemovePointerFromNode from to i i False

class RemovePointerFromNodes (from :: Ptr) (to :: Ptr) (i :: NodeList) (o :: NodeList) (tf :: Type) | from to i -> o tf

instance removePointerFromNodesNil :: RemovePointerFromNodes a b NodeListNil NodeListNil False

instance removePointerFromNodesCons ::
  ( RemovePointerFromNode a b head headRes tf0
  , RemovePointerFromNodes a b tail tailRes tf1
  , Or tf0 tf1 fin
  ) =>
  RemovePointerFromNodes a b (NodeListCons head tail) (NodeListCons headRes tailRes) fin

class Disconnect (from :: Ptr) (to :: Ptr) (i :: Universe) (o :: Universe) | from to i -> o where
  disconnect :: forall env proof. AudioUnitRef from -> AudioUnitRef to -> Frame env proof i o Unit

instance disconnector ::
  ( BinToInt from
  , BinToInt to
  , GraphToNodeList graphi nodeListI
  , RemovePointerFromNodes from to nodeListI nodeListO True
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
