module WAGS.Destroy where

import Prelude

import Control.Monad.State (modify_)
import Data.Map as M
import Data.Typelevel.Bool (False)
import WAGS.Control.Types (Frame(..))
import WAGS.Rendered (Instruction(..))
import WAGS.Universe.AudioUnit (class GetPointer, AudioUnitRef(..), TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Universe.Bin (class BinEq, class BinToInt, Ptr, PtrListCons)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList)
import WAGS.Universe.Node (class GetAudioUnit, Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (Universe, UniverseC)
import WAGS.Util (class Gate)
import WAGS.Validation (class PtrNotInPtrList)

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

instance removePtrFromNListCons ::
  ( GetAudioUnit head headAu
  , GetPointer headAu headPtr
  , BinEq ptr headPtr tf
  , RemovePtrFromNodeList ptr tail newTail
  , Gate tf newTail (NodeListCons head newTail) o
  ) =>
  RemovePtrFromNodeList ptr (NodeListCons head tail) o

class Destroy (ptr :: Ptr) (i :: Universe) (o :: Universe) | ptr i -> o where
  destroy :: forall env proof. AudioUnitRef ptr -> Frame env proof i o Unit

instance destroyer ::
  ( BinToInt ptr
  , GraphToNodeList graphi nodeListI
  , PointerNotConnecteds ptr nodeListI
  , RemovePtrFromNodeList ptr nodeListI nodeListO
  , GraphToNodeList grapho nodeListO
  ) =>
  Destroy ptr (UniverseC x graphi changeBit skolems) (UniverseC x grapho changeBit skolems) where
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
