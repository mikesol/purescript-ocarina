module Test.Main where

import Prelude

import Data.Typelevel.Bool (True, False)
import Data.Typelevel.Num (class Succ, D0, D1, D2, D3)
import Effect (Effect)
import Effect.Class.Console (log)
import Stream8 (class AllEdgesPointToNodes, class AudioUnitEq, class Gate, class HasBottomLevelNodes, class Lookup, class NoNodesAreDuplicated, class NoParallelEdges, class PtrEq, class UniqueTerminus, type (+:), type (/->), type (/:), AudioUnitRef, Changing, GraphC, Highpass(..), ManyEdges, NoEdge, NodeC, NodeListCons, NodeListNil, PtrListCons, PtrListNil, Gain (..), Scene, SinOsc(..), SingleEdge, Static, TGain, THighpass, TSinOsc, UniverseC, create)
import Data.Tuple.Nested
import Type.Proxy (Proxy(..))

---------------------------
----- tests
testPtrEq :: Proxy True
testPtrEq =
  Proxy ::
    forall tf.
    PtrEq D0 D0 tf =>
    Proxy tf

testPtrEq2 :: Proxy False
testPtrEq2 =
  Proxy ::
    forall tf.
    PtrEq D0 D1 tf =>
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
    AudioUnitEq (TSinOsc D0 Changing) (TSinOsc D0 Static) tf =>
    Proxy tf

testAudioUnitEq2 :: Proxy True
testAudioUnitEq2 =
  Proxy ::
    forall tf.
    AudioUnitEq (TSinOsc D0 Changing) (TSinOsc D0 Changing) tf =>
    Proxy tf

testLookup :: Proxy (NodeC (TSinOsc D0 Changing) NoEdge)
testLookup =
  Proxy ::
    forall node.
    Lookup D0 (GraphC (NodeC (TSinOsc D0 Changing) NoEdge) NodeListNil) node =>
    Proxy node

testLookup2 :: Proxy (NodeC (TSinOsc D1 Changing) NoEdge)
testLookup2 =
  Proxy ::
    forall node.
    Lookup D1 (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) NodeListNil) node =>
    Proxy node

testLookup3 :: Proxy (NodeC (TSinOsc D1 Changing) NoEdge)
testLookup3 =
  Proxy ::
    forall node.
    Lookup D1 (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (TSinOsc D0 Static) NoEdge) NodeListNil)) node =>
    Proxy node

testLookup4 :: Proxy (NodeC (TSinOsc D0 Static) NoEdge)
testLookup4 =
  Proxy ::
    forall node.
    Lookup D0 (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (TSinOsc D0 Static) NoEdge) NodeListNil)) node =>
    Proxy node

testLookup5 :: Proxy (NodeC (TSinOsc D0 Static) NoEdge)
testLookup5 =
  Proxy ::
    forall node.
    Lookup D0 (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (TSinOsc D0 Static) NoEdge) (NodeListCons (NodeC (THighpass D2 Static Static) NoEdge) NodeListNil))) node =>
    Proxy node

testNoNodesAreDuplicated :: Proxy (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (TSinOsc D0 Static) NoEdge) (NodeListCons (NodeC (THighpass D2 Static Static) NoEdge) NodeListNil)))
testNoNodesAreDuplicated =
  Proxy ::
    forall node.
    NoNodesAreDuplicated node =>
    Proxy node

-- AllEdgesPointToNodes
testAllEdgesPointToNodes :: Proxy (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (TSinOsc D0 Static) NoEdge) (NodeListCons (NodeC (THighpass D2 Static Static) (SingleEdge D2)) NodeListNil)))
testAllEdgesPointToNodes =
  Proxy ::
    forall node.
    AllEdgesPointToNodes node =>
    Proxy node

testAllEdgesPointToNodes2 :: Proxy (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (TSinOsc D0 Static) NoEdge) (NodeListCons (NodeC (TGain D2 Static) (ManyEdges D2 (PtrListCons D0 PtrListNil))) NodeListNil)))
testAllEdgesPointToNodes2 =
  Proxy ::
    forall node.
    AllEdgesPointToNodes node =>
    Proxy node

-- NoParallelEdges
testNoParallelEdges :: Proxy (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (TSinOsc D0 Static) NoEdge) (NodeListCons (NodeC (THighpass D2 Static Static) (SingleEdge D2)) NodeListNil)))
testNoParallelEdges =
  Proxy ::
    forall node.
    NoParallelEdges node =>
    Proxy node

testNoParallelEdges2 :: Proxy (GraphC ((TSinOsc D1 Changing) /-> NoEdge) (((TSinOsc D0 Static) /-> NoEdge) /: ((TGain D2 Static) /-> (ManyEdges D2 (D0 +: PtrListNil))) /: NodeListNil))
testNoParallelEdges2 =
  Proxy ::
    forall node.
    NoParallelEdges node =>
    Proxy node

-- HasBottomLevelNodes
testHasBottomLevelNodes :: Proxy (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (THighpass D2 Static Static) (SingleEdge D1)) NodeListNil))
testHasBottomLevelNodes =
  Proxy ::
    forall node.
    HasBottomLevelNodes node => Proxy node

-- UniqueTerminus
testUniqueTerminus :: Proxy (NodeC (THighpass D2 Static Static) (SingleEdge D1))
testUniqueTerminus =
  Proxy ::
    forall node.
    UniqueTerminus (GraphC (NodeC (TSinOsc D1 Changing) NoEdge) (NodeListCons (NodeC (THighpass D2 Static Static) (SingleEdge D1)) NodeListNil)) node =>
    Proxy node

testUniqueTerminus2 :: Proxy (TGain D3 Static /-> ManyEdges D0 (D2 +: PtrListNil))
testUniqueTerminus2 =
  Proxy ::
    forall node.
    UniqueTerminus
      ( GraphC
          (TGain D0 Static /-> ManyEdges D0 (D1 +: D2 +: PtrListNil))
          ( ((THighpass D1 Static Static) /-> (SingleEdge D0))
              /: ((TSinOsc D2 Static) /-> NoEdge)
              /: (TGain D3 Static /-> ManyEdges D0 (D2 +: PtrListNil))
              /: NodeListNil
          )
      )
      node =>
    Proxy node

createTest1 ::
  forall ptr next env destroyed head tail acc.
  Succ ptr next =>
  Scene env acc (UniverseC ptr (GraphC head tail) destroyed acc)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr Changing) NoEdge) (NodeListCons head tail))
        destroyed
        acc
    )
    (AudioUnitRef ptr)
createTest1 = create (SinOsc 440.0)

createTest2 ::
  forall first mid last env destroyed head tail acc.
  Succ first mid =>
  Succ mid last =>
  Scene env acc (UniverseC first (GraphC head tail) destroyed acc)
    ( UniverseC last
        (GraphC (NodeC (THighpass first Changing Changing) (SingleEdge mid)) (NodeListCons (NodeC (TSinOsc mid Changing) NoEdge) (NodeListCons head tail)))
        destroyed
        acc
    )
    (AudioUnitRef first)
createTest2 = create (Highpass 440.0 1.0 (SinOsc 440.0))

{-
createTest3 ::
  forall first mid last env destroyed head tail acc.
  Succ first mid =>
  Succ mid last =>
  Scene env acc (UniverseC first (GraphC head tail) destroyed acc)
    ( UniverseC last
        (GraphC (NodeC (THighpass first Changing Changing) (SingleEdge mid)) (NodeListCons (NodeC (TSinOsc mid Changing) NoEdge) (NodeListCons head tail)))
        destroyed
        acc
    )
    ( AudioUnitRef first)
-}

data MyGain

createTest3 = create (Gain 1.0 (\(gain :: Proxy MyGain) -> gain /\ SinOsc 440.0 /\ unit))

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
