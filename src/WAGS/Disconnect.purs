module WAGS.Disconnect where

import Prelude

import Control.Monad.State (modify_)
import Data.Map as M
import Data.Set as S
import Data.Typelevel.Bool (class Or, False, True)
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Interpret (class AudioInterpret, disconnectXFromY)
import WAGS.Universe.AudioUnit (AudioUnitRef(..))
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinEq, class BinToInt, Ptr, PtrList, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, Graph)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Util (class Gate)

-- | Disconnect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class Disconnect (source :: Ptr) (dest :: Ptr) (i :: Graph) (o :: Graph) | source dest i -> o where
  disconnect :: forall env audio engine proof m res currentIdx changeBit skolems. Monad m => AudioInterpret audio engine => AudioUnitRef source -> AudioUnitRef dest -> FrameT env audio engine proof m res (UniverseC currentIdx i changeBit skolems) (UniverseC currentIdx o changeBit skolems) Unit

instance disconnector ::
  ( BinToInt from
  , BinToInt to
  , GraphToNodeList graphi nodeListI
  , RemovePointerFromNodes from to nodeListI nodeListO True
  , GraphToNodeList grapho nodeListO
  ) =>
  Disconnect from to graphi grapho where
  disconnect (AudioUnitRef fromI) (AudioUnitRef toI) =
    unsafeFrame
      $ do
          modify_
            ( \i ->
                i
                  { internalEdges = M.insertWith S.difference toI (S.singleton fromI) (i.internalEdges)
                  , instructions = i.instructions <> [ disconnectXFromY fromI toI ]
                  }
            )

-- | Internal helper class used for disconnecting.
class RemovePtrFromList (ptr :: Ptr) (i :: PtrList) (o :: PtrList) | ptr i -> o

instance removePtrFromListNil :: RemovePtrFromList ptr PtrListNil PtrListNil

instance removePtrFromListCons ::
  ( BinEq ptr head tf
  , RemovePtrFromList ptr tail newTail
  , Gate tf newTail (PtrListCons head newTail) o
  ) =>
  RemovePtrFromList ptr (PtrListCons head tail) o

-- | Internal helper class used for disconnecting.
class RemovePointerFromNode (from :: Ptr) (to :: Ptr) (i :: Node) (o :: Node) (tf :: Type) | from to i -> o tf

instance removePointerFromNodeAllpassHitSE :: RemovePointerFromNode from to (NodeC (AU.TAllpass to) (SingleEdge from)) (NodeC (AU.TAllpass to) NoEdge) True
else instance removePointerFromNodeBandpassHitSE :: RemovePointerFromNode from to (NodeC (AU.TBandpass to) (SingleEdge from)) (NodeC (AU.TBandpass to) NoEdge) True
else instance removePointerFromNodeConvolverHitSE :: RemovePointerFromNode from to (NodeC (AU.TConvolver to name) (SingleEdge from)) (NodeC (AU.TConvolver to name) NoEdge) True
else instance removePointerFromNodeDelayHitSE :: RemovePointerFromNode from to (NodeC (AU.TDelay to) (SingleEdge from)) (NodeC (AU.TDelay to) NoEdge) True
else instance removePointerFromNodeDynamicsCompressorHitSE :: RemovePointerFromNode from to (NodeC (AU.TDynamicsCompressor to) (SingleEdge from)) (NodeC (AU.TDynamicsCompressor to) NoEdge) True
else instance removePointerFromNodeGainHitSE :: RemovePointerFromNode from to (NodeC (AU.TGain to) (SingleEdge from)) (NodeC (AU.TGain to) NoEdge) True
else instance removePointerFromNodeGainHitMEC :: (RemovePtrFromList from (PtrListCons e (PtrListCons l (PtrListCons r0 r1))) (PtrListCons head tail)) => RemovePointerFromNode from to (NodeC (AU.TGain to) (ManyEdges e (PtrListCons l (PtrListCons r0 r1)))) (NodeC (AU.TGain to) (ManyEdges head tail)) True
else instance removePointerFromNodeGainHitMEN :: (RemovePtrFromList from (PtrListCons e (PtrListCons l PtrListNil)) (PtrListCons head PtrListNil)) => RemovePointerFromNode from to (NodeC (AU.TGain to) (ManyEdges e (PtrListCons l PtrListNil))) (NodeC (AU.TGain to) (SingleEdge head)) True
else instance removePointerFromNodeHighpassHitSE :: RemovePointerFromNode from to (NodeC (AU.THighpass to) (SingleEdge from)) (NodeC (AU.THighpass to) NoEdge) True
else instance removePointerFromNodeHighshelfHitSE :: RemovePointerFromNode from to (NodeC (AU.THighshelf to) (SingleEdge from)) (NodeC (AU.THighshelf to) NoEdge) True
else instance removePointerFromNodeLowpassHitSE :: RemovePointerFromNode from to (NodeC (AU.TLowpass to) (SingleEdge from)) (NodeC (AU.TLowpass to) NoEdge) True
else instance removePointerFromNodeLowshelfHitSE :: RemovePointerFromNode from to (NodeC (AU.TLowshelf to) (SingleEdge from)) (NodeC (AU.TLowshelf to) NoEdge) True
else instance removePointerFromNodeNotchHitSE :: RemovePointerFromNode from to (NodeC (AU.TNotch to) (SingleEdge from)) (NodeC (AU.TNotch to) NoEdge) True
else instance removePointerFromNodePeakingHitSE :: RemovePointerFromNode from to (NodeC (AU.TPeaking to) (SingleEdge from)) (NodeC (AU.TPeaking to) NoEdge) True
else instance removePointerFromNodeRecorderHitSE :: RemovePointerFromNode from to (NodeC (AU.TRecorder to name) (SingleEdge from)) (NodeC (AU.TRecorder to name) NoEdge) True
else instance removePointerFromNodeSpeakerHitSE :: RemovePointerFromNode from to (NodeC (AU.TSpeaker to) (SingleEdge from)) (NodeC (AU.TSpeaker to) NoEdge) True
else instance removePointerFromNodeSpeakerHitMEC :: (RemovePtrFromList from (PtrListCons e (PtrListCons l (PtrListCons r0 r1))) (PtrListCons head tail)) => RemovePointerFromNode from to (NodeC (AU.TSpeaker to) (ManyEdges e (PtrListCons l (PtrListCons r0 r1)))) (NodeC (AU.TSpeaker to) (ManyEdges head tail)) True
else instance removePointerFromNodeSpeakerHitMEN :: (RemovePtrFromList from (PtrListCons e (PtrListCons l PtrListNil)) (PtrListCons head PtrListNil)) => RemovePointerFromNode from to (NodeC (AU.TSpeaker to) (ManyEdges e (PtrListCons l PtrListNil))) (NodeC (AU.TSpeaker to) (SingleEdge head)) True
else instance removePointerFromNodeStereoPannerHitSE :: RemovePointerFromNode from to (NodeC (AU.TStereoPanner to) (SingleEdge from)) (NodeC (AU.TStereoPanner to) NoEdge) True
else instance removePointerFromNodeWaveShaperHitSE :: RemovePointerFromNode from to (NodeC (AU.TWaveShaper to name) (SingleEdge from)) (NodeC (AU.TWaveShaper to name) NoEdge) True
else instance removePointerFromNodeMiss :: RemovePointerFromNode from to i i False

-- | Internal helper class used for disconnecting.
class RemovePointerFromNodes (from :: Ptr) (to :: Ptr) (i :: NodeList) (o :: NodeList) (tf :: Type) | from to i -> o tf

instance removePointerFromNodesNil :: RemovePointerFromNodes a b NodeListNil NodeListNil False

instance removePointerFromNodesCons ::
  ( RemovePointerFromNode a b head headRes tf0
  , RemovePointerFromNodes a b tail tailRes tf1
  , Or tf0 tf1 fin
  ) =>
  RemovePointerFromNodes a b (NodeListCons head tail) (NodeListCons headRes tailRes) fin
