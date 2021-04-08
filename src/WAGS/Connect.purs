module WAGS.Connect where

import Prelude
import Control.Monad.State (modify_)
import Data.Map as M
import Data.Set as S
import Data.Typelevel.Bool (class Or, False, True)
import WAGS.Control.Types (FrameT(..))
import WAGS.Rendered (Instruction(..))
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.AudioUnit (AudioUnitRef(..))
import WAGS.Universe.Bin (class BinToInt, Ptr, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (Universe, UniverseC)

class AddPointerToNode (from :: Ptr) (to :: Ptr) (i :: Node) (o :: Node) (tf :: Type) | from to i -> o tf

instance addPointerToNodeAllpassHitSE :: AddPointerToNode from to (NodeC (AU.TAllpass to) (SingleEdge e)) (NodeC (AU.TAllpass to) (SingleEdge from)) True
else instance addPointerToNodeBandpassHitSE :: AddPointerToNode from to (NodeC (AU.TBandpass to) (SingleEdge e)) (NodeC (AU.TBandpass to) (SingleEdge from)) True
else instance addPointerToNodeConvolverHitSE :: AddPointerToNode from to (NodeC (AU.TConvolver to) (SingleEdge e)) (NodeC (AU.TConvolver to) (SingleEdge from)) True
else instance addPointerToNodeDelayHitSE :: AddPointerToNode from to (NodeC (AU.TDelay to) (SingleEdge e)) (NodeC (AU.TDelay to) (SingleEdge from)) True
else instance addPointerToNodeDynamicsCompressorHitSE :: AddPointerToNode from to (NodeC (AU.TDynamicsCompressor to) (SingleEdge e)) (NodeC (AU.TDynamicsCompressor to) (SingleEdge from)) True
else instance addPointerToNodeGainHitSE :: AddPointerToNode from to (NodeC (AU.TGain to) (SingleEdge e)) (NodeC (AU.TGain to) (ManyEdges from (PtrListCons e PtrListNil))) True
else instance addPointerToNodeGainHitME :: AddPointerToNode from to (NodeC (AU.TGain to) (ManyEdges e l)) (NodeC (AU.TGain to) (ManyEdges from (PtrListCons e l))) True
else instance addPointerToNodeHighpassHitSE :: AddPointerToNode from to (NodeC (AU.THighpass to) (SingleEdge e)) (NodeC (AU.THighpass to) (SingleEdge from)) True
else instance addPointerToNodeHighshelfHitSE :: AddPointerToNode from to (NodeC (AU.THighshelf to) (SingleEdge e)) (NodeC (AU.THighshelf to) (SingleEdge from)) True
else instance addPointerToNodeLowpassHitSE :: AddPointerToNode from to (NodeC (AU.TLowpass to) (SingleEdge e)) (NodeC (AU.TLowpass to) (SingleEdge from)) True
else instance addPointerToNodeLowshelfHitSE :: AddPointerToNode from to (NodeC (AU.TLowshelf to) (SingleEdge e)) (NodeC (AU.TLowshelf to) (SingleEdge from)) True
else instance addPointerToNodeNotchHitSE :: AddPointerToNode from to (NodeC (AU.TNotch to) (SingleEdge e)) (NodeC (AU.TNotch to) (SingleEdge from)) True
else instance addPointerToNodePeakingHitSE :: AddPointerToNode from to (NodeC (AU.TPeaking to) (SingleEdge e)) (NodeC (AU.TPeaking to) (SingleEdge from)) True
else instance addPointerToNodeRecorderHitSE :: AddPointerToNode from to (NodeC (AU.TRecorder to) (SingleEdge e)) (NodeC (AU.TRecorder to) (SingleEdge from)) True
else instance addPointerToNodeSpeakerHitSE :: AddPointerToNode from to (NodeC (AU.TSpeaker to) (SingleEdge e)) (NodeC (AU.TSpeaker to) (ManyEdges from (PtrListCons e PtrListNil))) True
else instance addPointerToNodeSpeakerHitME :: AddPointerToNode from to (NodeC (AU.TSpeaker to) (ManyEdges e l)) (NodeC (AU.TSpeaker to) (ManyEdges from (PtrListCons e l))) True
else instance addPointerToNodeStereoPannerHitSE :: AddPointerToNode from to (NodeC (AU.TStereoPanner to) (SingleEdge e)) (NodeC (AU.TStereoPanner to) (SingleEdge from)) True
else instance addPointerToNodeWaveShaperHitSE :: AddPointerToNode from to (NodeC (AU.TWaveShaper to) (SingleEdge e)) (NodeC (AU.TWaveShaper to) (SingleEdge from)) True
else instance addPointerToNodeMiss :: AddPointerToNode from to i i False

class AddPointerToNodes (from :: Ptr) (to :: Ptr) (i :: NodeList) (o :: NodeList) (tf :: Type) | from to i -> o tf

instance addPointerToNodesNil :: AddPointerToNodes a b NodeListNil NodeListNil False

instance addPointerToNodesCons :: (AddPointerToNode a b head headRes tf0, AddPointerToNodes a b tail tailRes tf1, Or tf0 tf1 fin) => AddPointerToNodes a b (NodeListCons head tail) (NodeListCons headRes tailRes) fin

class Connect (from :: Ptr) (to :: Ptr) (i :: Universe) (o :: Universe) | from to i -> o where
  connect :: forall env proof m. Monad m => AudioUnitRef from -> AudioUnitRef to -> FrameT env proof m i o Unit

instance connectAll ::
  ( BinToInt from
  , BinToInt to
  , GraphToNodeList graphi nodeListI
  , AddPointerToNodes from to nodeListI nodeListO True
  , GraphToNodeList grapho nodeListO
  ) =>
  Connect from to (UniverseC ptr graphi changeBit skolems) (UniverseC ptr grapho changeBit skolems) where
  connect (AudioUnitRef fromI) (AudioUnitRef toI) =
    FrameT
      $ do
          modify_
            ( \i ->
                i
                  { internalEdges = (M.insertWith S.union toI (S.singleton fromI) i.internalEdges)
                  , instructions = i.instructions <> [ ConnectXToY fromI toI ]
                  }
            )
