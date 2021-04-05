module WAGS.Connect where

import Prelude

import Control.Monad.State (modify_)
import Data.Map as M
import Data.Set as S
import WAGS.Control.Types (Frame(..))
import WAGS.Rendered (Instruction(..))
import WAGS.Universe.AudioUnit (AudioUnitRef(..), TGain, THighpass, TSpeaker)
import WAGS.Universe.Bin (class BinToInt, Ptr, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (Universe, UniverseC)

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

instance connectAll ::
  ( BinToInt from
  , BinToInt to
  , GraphToNodeList graphi nodeListI
  , AddPointerToNodes from to nodeListI nodeListO
  , GraphToNodeList grapho nodeListO
  ) =>
  Connect from to (UniverseC ptr graphi skolems) (UniverseC ptr grapho skolems) where
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
