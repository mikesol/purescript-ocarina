module WAGS.Disconnect where

import Prelude

import Control.Monad.State (modify_)
import Data.Map as M
import Data.Set as S
import Data.Typelevel.Bool (class Or, False, True)
import WAGS.Control.Types (FrameT(..))
import WAGS.Interpret (class AudioInterpret, disconnectXFromY)
import WAGS.Universe.AudioUnit (AudioUnitRef(..))
import WAGS.Universe.AudioUnit as AU
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

instance removePointerFromNodeAllpassHitSE :: RemovePointerFromNode from to (NodeC (AU.TAllpass to) (SingleEdge from)) (NodeC (AU.TAllpass to) NoEdge) True
else instance removePointerFromNodeBandpassHitSE :: RemovePointerFromNode from to (NodeC (AU.TBandpass to) (SingleEdge from)) (NodeC (AU.TBandpass to) NoEdge) True
else instance removePointerFromNodeConvolverHitSE :: RemovePointerFromNode from to (NodeC (AU.TConvolver to) (SingleEdge from)) (NodeC (AU.TConvolver to) NoEdge) True
else instance removePointerFromNodeDelayHitSE :: RemovePointerFromNode from to (NodeC (AU.TDelay to) (SingleEdge from)) (NodeC (AU.TDelay to) NoEdge) True
else instance removePointerFromNodeDynamicsCompressorHitSE :: RemovePointerFromNode from to (NodeC (AU.TDynamicsCompressor to) (SingleEdge from)) (NodeC (AU.TDynamicsCompressor to) NoEdge) True
else instance removePointerFromNodeGainHitSE :: RemovePointerFromNode from to (NodeC (AU.TGain to) (SingleEdge from)) (NodeC (AU.TGain to) NoEdge) True
else instance removePointerFromNodeGainHitME :: (RemovePtrFromList from (PtrListCons e (PtrListCons l r)) (PtrListCons head tail)) => RemovePointerFromNode from to (NodeC (AU.TGain to) (ManyEdges e (PtrListCons l r))) (NodeC (AU.TGain to) (ManyEdges head tail)) True
else instance removePointerFromNodeHighpassHitSE :: RemovePointerFromNode from to (NodeC (AU.THighpass to) (SingleEdge from)) (NodeC (AU.THighpass to) NoEdge) True
else instance removePointerFromNodeHighshelfHitSE :: RemovePointerFromNode from to (NodeC (AU.THighshelf to) (SingleEdge from)) (NodeC (AU.THighshelf to) NoEdge) True
else instance removePointerFromNodeLowpassHitSE :: RemovePointerFromNode from to (NodeC (AU.TLowpass to) (SingleEdge from)) (NodeC (AU.TLowpass to) NoEdge) True
else instance removePointerFromNodeLowshelfHitSE :: RemovePointerFromNode from to (NodeC (AU.TLowshelf to) (SingleEdge from)) (NodeC (AU.TLowshelf to) NoEdge) True
else instance removePointerFromNodeNotchHitSE :: RemovePointerFromNode from to (NodeC (AU.TNotch to) (SingleEdge from)) (NodeC (AU.TNotch to) NoEdge) True
else instance removePointerFromNodePeakingHitSE :: RemovePointerFromNode from to (NodeC (AU.TPeaking to) (SingleEdge from)) (NodeC (AU.TPeaking to) NoEdge) True
else instance removePointerFromNodeRecorderHitSE :: RemovePointerFromNode from to (NodeC (AU.TRecorder to) (SingleEdge from)) (NodeC (AU.TRecorder to) NoEdge) True
else instance removePointerFromNodeSpeakerHitSE :: RemovePointerFromNode from to (NodeC (AU.TSpeaker to) (SingleEdge from)) (NodeC (AU.TSpeaker to) NoEdge) True
else instance removePointerFromNodeSpeakerHitME :: (RemovePtrFromList from (PtrListCons e (PtrListCons l r)) (PtrListCons head tail)) => RemovePointerFromNode from to (NodeC (AU.TSpeaker to) (ManyEdges e (PtrListCons l r))) (NodeC (AU.TSpeaker to) (ManyEdges head tail)) True
else instance removePointerFromNodeStereoPannerHitSE :: RemovePointerFromNode from to (NodeC (AU.TStereoPanner to) (SingleEdge from)) (NodeC (AU.TStereoPanner to) NoEdge) True
else instance removePointerFromNodeWaveShaperHitSE :: RemovePointerFromNode from to (NodeC (AU.TWaveShaper to) (SingleEdge from)) (NodeC (AU.TWaveShaper to) NoEdge) True
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
  disconnect :: forall env audio engine proof m. Monad m => AudioInterpret audio engine => AudioUnitRef from -> AudioUnitRef to -> FrameT env audio engine  proof m i o Unit

instance disconnector ::
  ( BinToInt from
  , BinToInt to
  , GraphToNodeList graphi nodeListI
  , RemovePointerFromNodes from to nodeListI nodeListO True
  , GraphToNodeList grapho nodeListO
  ) =>
  Disconnect from to (UniverseC ptr graphi changeBit skolems) (UniverseC ptr grapho changeBit skolems) where
  disconnect (AudioUnitRef fromI) (AudioUnitRef toI) =
    FrameT
      $ do
          modify_
            ( \i ->
                i
                  { internalEdges = M.insertWith S.difference toI (S.singleton fromI) (i.internalEdges)
                  , instructions = i.instructions <> [ disconnectXFromY fromI toI ]
                  }
            )
