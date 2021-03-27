module Test.Main where

import Prelude
import Data.Typelevel.Bool (True, False)
import Effect (Effect)
import Effect.Class.Console (log)
import Stream8 (class AllEdgesPointToNodes, class AudioUnitEq, class Gate, class HasBottomLevelNodes, class NoNodesAreDuplicated, class NoParallelEdges, class PtrEq, class UniqueTerminus, type (+:), type (/->), type (/:), Changing, GraphC, ManyEdges, NoEdge, NodeC, NodeListCons, NodeListNil, PtrListCons, PtrListNil, PtrSucc, PtrZ, SingleEdge, Static, TGain, THighpass, TSinOsc)
import Type.Proxy (Proxy(..))

---------------------------
----- tests
testPtrEq :: Proxy True
testPtrEq =
  Proxy ::
    forall tf.
    PtrEq PtrZ PtrZ tf =>
    Proxy tf

testPtrEq2 :: Proxy False
testPtrEq2 =
  Proxy ::
    forall tf.
    PtrEq PtrZ (PtrSucc PtrZ) tf =>
    Proxy tf

testGate :: Proxy "a"
testGate =
  Proxy ::
    forall s.
    Gate True "a" "b" s =>
    Proxy s

testGate2 :: Proxy "b"
testGate2 =
  Proxy ::
    forall s.
    Gate False "a" "b" s =>
    Proxy s

testAudioUnitEq :: Proxy False
testAudioUnitEq =
  Proxy ::
    forall tf.
    AudioUnitEq (TSinOsc PtrZ Changing) (TSinOsc PtrZ Static) tf =>
    Proxy tf

testAudioUnitEq2 :: Proxy True
testAudioUnitEq2 =
  Proxy ::
    forall tf.
    AudioUnitEq (TSinOsc PtrZ Changing) (TSinOsc PtrZ Changing) tf =>
    Proxy tf

testNoNodesAreDuplicated :: Proxy (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (TSinOsc PtrZ Static) NoEdge) (NodeListCons (NodeC (THighpass (PtrSucc (PtrSucc PtrZ)) Static Static) NoEdge) NodeListNil)))
testNoNodesAreDuplicated =
  Proxy ::
    forall node.
    NoNodesAreDuplicated node =>
    Proxy node

-- AllEdgesPointToNodes
testAllEdgesPointToNodes :: Proxy (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (TSinOsc PtrZ Static) NoEdge) (NodeListCons (NodeC (THighpass (PtrSucc (PtrSucc PtrZ)) Static Static) (SingleEdge (PtrSucc (PtrSucc PtrZ)))) NodeListNil)))
testAllEdgesPointToNodes =
  Proxy ::
    forall node.
    AllEdgesPointToNodes node =>
    Proxy node

testAllEdgesPointToNodes2 :: Proxy (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (TSinOsc PtrZ Static) NoEdge) (NodeListCons (NodeC (TGain (PtrSucc (PtrSucc PtrZ)) Static) (ManyEdges (PtrSucc (PtrSucc PtrZ)) (PtrListCons PtrZ PtrListNil))) NodeListNil)))
testAllEdgesPointToNodes2 =
  Proxy ::
    forall node.
    AllEdgesPointToNodes node =>
    Proxy node

-- NoParallelEdges
testNoParallelEdges :: Proxy (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (TSinOsc PtrZ Static) NoEdge) (NodeListCons (NodeC (THighpass (PtrSucc (PtrSucc PtrZ)) Static Static) (SingleEdge (PtrSucc (PtrSucc PtrZ)))) NodeListNil)))
testNoParallelEdges =
  Proxy ::
    forall node.
    NoParallelEdges node =>
    Proxy node

testNoParallelEdges2 :: Proxy (GraphC ((TSinOsc (PtrSucc PtrZ) Changing) /-> NoEdge) (((TSinOsc PtrZ Static) /-> NoEdge) /: ((TGain (PtrSucc (PtrSucc PtrZ)) Static) /-> (ManyEdges (PtrSucc (PtrSucc PtrZ)) (PtrZ +: PtrListNil))) /: NodeListNil))
testNoParallelEdges2 =
  Proxy ::
    forall node.
    NoParallelEdges node =>
    Proxy node

-- HasBottomLevelNodes
testHasBottomLevelNodes :: Proxy (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (THighpass (PtrSucc (PtrSucc PtrZ)) Static Static) (SingleEdge (PtrSucc PtrZ))) NodeListNil))
testHasBottomLevelNodes =
  Proxy ::
    forall node.
    HasBottomLevelNodes node => Proxy node

-- UniqueTerminus

testUniqueTerminus :: Proxy (NodeC (THighpass (PtrSucc (PtrSucc PtrZ)) Static Static) (SingleEdge (PtrSucc PtrZ)))
testUniqueTerminus =
  Proxy ::
    forall node.
    UniqueTerminus (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (THighpass (PtrSucc (PtrSucc PtrZ)) Static Static) (SingleEdge (PtrSucc PtrZ))) NodeListNil)) node =>
    Proxy node

testUniqueTerminus2 :: Proxy (TGain (PtrSucc (PtrSucc (PtrSucc PtrZ))) Static /-> ManyEdges PtrZ (PtrSucc (PtrSucc PtrZ) +: PtrListNil))
testUniqueTerminus2 =
  Proxy ::
    forall node.
    UniqueTerminus
      ( GraphC
          (TGain PtrZ Static /-> ManyEdges PtrZ (PtrSucc PtrZ +: PtrSucc (PtrSucc PtrZ) +: PtrListNil))
          ( ( (THighpass (PtrSucc PtrZ) Static Static) /-> (SingleEdge PtrZ))
              /: ((TSinOsc (PtrSucc (PtrSucc PtrZ)) Static) /-> NoEdge)
              /: (TGain (PtrSucc (PtrSucc (PtrSucc PtrZ))) Static /-> ManyEdges PtrZ (PtrSucc (PtrSucc PtrZ) +: PtrListNil))
              /: NodeListNil
          )
      )
      node =>
    Proxy node

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
