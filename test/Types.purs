module Test.WAGS.Types where

import Data.Typelevel.Bool (False, True)
import Type.Proxy (Proxy(..))
import WAGS (class AllEdgesPointToNodes, class AudioUnitEq, class BinEq, class BinSub, class BinSucc, class HasBottomLevelNodes, class LookupNL, class NoNodesAreDuplicated, class NoParallelEdges, class UniqueTerminus, type (+:), type (/->), type (/:), Bc, Bn, D0, D1, D2, D3, GraphC, I, ManyEdges, NoEdge, NodeC, NodeListCons, NodeListNil, O, PtrListCons, PtrListNil, SingleEdge, TGain, THighpass, TSinOsc)
import WAGS.Util (class Gate)

testBinSucc0 :: Proxy (Bc I Bn)
testBinSucc0 =
  Proxy ::
    forall b.
    BinSucc (Bc O Bn) b =>
    Proxy b

testBinSucc1 :: Proxy (Bc O (Bc I Bn))
testBinSucc1 =
  Proxy ::
    forall b.
    BinSucc (Bc I Bn) b =>
    Proxy b

testBinSucc2 :: Proxy (Bc I (Bc I Bn))
testBinSucc2 =
  Proxy ::
    forall b.
    BinSucc (Bc O (Bc I Bn)) b =>
    Proxy b

testBinSucc3 :: Proxy (Bc O (Bc O (Bc I Bn)))
testBinSucc3 =
  Proxy ::
    forall b.
    BinSucc (Bc I (Bc I Bn)) b =>
    Proxy b

testBinSub3m0 :: Proxy (Bc I (Bc I Bn))
testBinSub3m0 =
  Proxy ::
    forall b.
    BinSub (Bc I (Bc I Bn)) (Bc O Bn) b =>
    Proxy b

testBinSub3m1 :: Proxy (Bc O (Bc I Bn))
testBinSub3m1 =
  Proxy ::
    forall b.
    BinSub (Bc I (Bc I Bn)) (Bc I Bn) b =>
    Proxy b

testBinSub8m6 :: Proxy (Bc O (Bc I Bn))
testBinSub8m6 =
  Proxy ::
    forall b.
    BinSub (Bc O (Bc O (Bc O (Bc I Bn)))) (Bc O (Bc I (Bc I Bn))) b =>
    Proxy b

testBinSub1 :: Proxy (Bc O (Bc I Bn))
testBinSub1 =
  Proxy ::
    forall b.
    BinSub (Bc I (Bc I Bn)) (Bc I Bn) b =>
    Proxy b

testBinEq :: Proxy True
testBinEq =
  Proxy ::
    forall tf.
    BinEq D0 D0 tf =>
    Proxy tf

testBinEq2 :: Proxy False
testBinEq2 =
  Proxy ::
    forall tf.
    BinEq D0 D1 tf =>
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

testAudioUnitEq :: Proxy True
testAudioUnitEq =
  Proxy ::
    forall tf.
    AudioUnitEq (TSinOsc D0) (TSinOsc D0) tf =>
    Proxy tf

testAudioUnitEq2 :: Proxy True
testAudioUnitEq2 =
  Proxy ::
    forall tf.
    AudioUnitEq (TSinOsc D0) (TSinOsc D0) tf =>
    Proxy tf

testLookupNL :: Proxy ((NodeC (TSinOsc D0) NoEdge) /: NodeListNil)
testLookupNL =
  Proxy ::
    forall node.
    LookupNL NodeListNil D0 (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil) node =>
    Proxy node

testLookupNL2 :: Proxy ((NodeC (TSinOsc D1) NoEdge) /: NodeListNil)
testLookupNL2 =
  Proxy ::
    forall node.
    LookupNL NodeListNil D1 (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil) node =>
    Proxy node

testLookupNL3 :: Proxy ((NodeC (TSinOsc D1) NoEdge) /: NodeListNil)
testLookupNL3 =
  Proxy ::
    forall node.
    LookupNL NodeListNil D1 (NodeListCons (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)) node =>
    Proxy node

testLookupNL4 :: Proxy ((NodeC (TSinOsc D0) NoEdge) /: NodeListNil)
testLookupNL4 =
  Proxy ::
    forall node.
    LookupNL NodeListNil D0 (NodeListCons (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)) node =>
    Proxy node

testLookupNL5 :: Proxy (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
testLookupNL5 =
  Proxy ::
    forall node.
    LookupNL NodeListNil D0 (NodeListCons (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) (NodeListCons (NodeC (THighpass D2) NoEdge) NodeListNil))) node =>
    Proxy node

testNoNodesAreDuplicated :: Proxy (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) (NodeListCons (NodeC (THighpass D2) NoEdge) NodeListNil)))
testNoNodesAreDuplicated =
  Proxy ::
    forall node.
    NoNodesAreDuplicated node =>
    Proxy node

-- AllEdgesPointToNodes
testAllEdgesPointToNodes :: Proxy (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) (NodeListCons (NodeC (THighpass D2) (SingleEdge D2)) NodeListNil)))
testAllEdgesPointToNodes =
  Proxy ::
    forall node.
    AllEdgesPointToNodes node =>
    Proxy node

testAllEdgesPointToNodes2 :: Proxy (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) (NodeListCons (NodeC (TGain D2) (ManyEdges D2 (PtrListCons D0 PtrListNil))) NodeListNil)))
testAllEdgesPointToNodes2 =
  Proxy ::
    forall node.
    AllEdgesPointToNodes node =>
    Proxy node

-- NoParallelEdges
testNoParallelEdges :: Proxy (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) (NodeListCons (NodeC (THighpass D2) (SingleEdge D2)) NodeListNil)))
testNoParallelEdges =
  Proxy ::
    forall node.
    NoParallelEdges node =>
    Proxy node

testNoParallelEdges2 :: Proxy (GraphC ((TSinOsc D1) /-> NoEdge) (((TSinOsc D0) /-> NoEdge) /: ((TGain D2) /-> (ManyEdges D2 (D0 +: PtrListNil))) /: NodeListNil))
testNoParallelEdges2 =
  Proxy ::
    forall node.
    NoParallelEdges node =>
    Proxy node

-- HasBottomLevelNodes
testHasBottomLevelNodes :: Proxy (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (THighpass D2) (SingleEdge D1)) NodeListNil))
testHasBottomLevelNodes =
  Proxy ::
    forall node.
    HasBottomLevelNodes node => Proxy node

-- UniqueTerminus
testUniqueTerminus :: Proxy (NodeC (THighpass D2) (SingleEdge D1))
testUniqueTerminus =
  Proxy ::
    forall node.
    UniqueTerminus (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (THighpass D2) (SingleEdge D1)) NodeListNil)) node =>
    Proxy node

testUniqueTerminus2 :: Proxy (TGain D3 /-> ManyEdges D0 (D2 +: PtrListNil))
testUniqueTerminus2 =
  Proxy ::
    forall node.
    UniqueTerminus
      ( GraphC
          (TGain D0 /-> ManyEdges D0 (D1 +: D2 +: PtrListNil))
          ( ((THighpass D1) /-> (SingleEdge D0))
              /: ((TSinOsc D2) /-> NoEdge)
              /: (TGain D3 /-> ManyEdges D0 (D2 +: PtrListNil))
              /: NodeListNil
          )
      )
      node =>
    Proxy node
