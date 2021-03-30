module Test.Main where

import Prelude

import Control.Applicative.Indexed (ipure, ivoid)
import Control.Monad.Indexed.Qualified as Ix
import Data.Array as A
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Set as S
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Bool (True, False)
import Data.Typelevel.Num (class Succ, D0, D1, D2, D3, D4)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Stream8 (class AllEdgesPointToNodes, class AudioUnitEq, class Gate, class HasBottomLevelNodes, class Lookup, class NoNodesAreDuplicated, class NoParallelEdges, class PtrEq, class UniqueTerminus, type (+:), type (/->), type (/:), AnAudioUnit(..), AudioParameter, AudioParameterTransition(..), AudioUnitRef(..), Dup(..), Focus(..), Frame, Gain(..), GraphC, Highpass(..), InitialGraph, Instruction(..), ManyEdges, NoEdge, NodeC, NodeListCons, NodeListNil, PtrListCons, PtrListNil, SinOsc(..), SingleEdge, SkolemListNil, Speaker(..), TGain, THighpass, TSinOsc, TSpeaker, UniverseC, create, cursor, destroy, disconnect, loop, makeChangingSceneLoop, oneFrame, param, start, testCompare)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
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

testLookup :: Proxy (NodeC (TSinOsc D0) NoEdge)
testLookup =
  Proxy ::
    forall node.
    Lookup D0 (GraphC (NodeC (TSinOsc D0) NoEdge) NodeListNil) node =>
    Proxy node

testLookup2 :: Proxy (NodeC (TSinOsc D1) NoEdge)
testLookup2 =
  Proxy ::
    forall node.
    Lookup D1 (GraphC (NodeC (TSinOsc D1) NoEdge) NodeListNil) node =>
    Proxy node

testLookup3 :: Proxy (NodeC (TSinOsc D1) NoEdge)
testLookup3 =
  Proxy ::
    forall node.
    Lookup D1 (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)) node =>
    Proxy node

testLookup4 :: Proxy (NodeC (TSinOsc D0) NoEdge)
testLookup4 =
  Proxy ::
    forall node.
    Lookup D0 (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)) node =>
    Proxy node

testLookup5 :: Proxy (NodeC (TSinOsc D0) NoEdge)
testLookup5 =
  Proxy ::
    forall node.
    Lookup D0 (GraphC (NodeC (TSinOsc D1) NoEdge) (NodeListCons (NodeC (TSinOsc D0) NoEdge) (NodeListCons (NodeC (THighpass D2) NoEdge) NodeListNil))) node =>
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

createTest1 ::
  forall ptr next env skolems head tail acc proof.
  Succ ptr next =>
  Frame env proof (UniverseC ptr (GraphC head tail) skolems acc)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr) NoEdge) (NodeListCons head tail))
        skolems
        acc
    )
    (AudioUnitRef ptr)
createTest1 = create (SinOsc 440.0)

createTest2 ::
  forall first mid last env skolems head tail acc proof.
  Succ first mid =>
  Succ mid last =>
  Frame env proof (UniverseC first (GraphC head tail) skolems acc)
    ( UniverseC last
        (GraphC (NodeC (THighpass first) (SingleEdge mid)) (NodeListCons (NodeC (TSinOsc mid) NoEdge) (NodeListCons head tail)))
        skolems
        acc
    )
    (AudioUnitRef first)
createTest2 = create (Highpass 440.0 1.0 (SinOsc 440.0))

data MyGain

createTest3 ::
  forall first mid last env head tail acc proof.
  Succ first mid =>
  Succ mid last =>
  Frame env proof (UniverseC first (GraphC head tail) SkolemListNil acc)
    ( UniverseC last
        ( GraphC
            ( NodeC (TGain first)
                (ManyEdges first (PtrListCons mid PtrListNil))
            )
            (NodeListCons (NodeC (TSinOsc mid) NoEdge) (NodeListCons head tail))
        )
        SkolemListNil
        acc
    )
    (AudioUnitRef first)
createTest3 = create (Gain 1.0 (\(gain :: Proxy MyGain) -> gain /\ SinOsc 440.0 /\ unit))

createTest4 ::
  forall first mid0 mid1 last env head tail acc proof.
  Succ first mid0 =>
  Succ mid0 mid1 =>
  Succ mid1 last =>
  Frame env proof (UniverseC first (GraphC head tail) SkolemListNil acc)
    ( UniverseC last
        ( GraphC
            ( NodeC (TGain first)
                (ManyEdges first (PtrListCons mid0 PtrListNil))
            )
            ( NodeListCons (NodeC (THighpass mid0) (SingleEdge mid1))
                (NodeListCons (NodeC (TSinOsc mid1) NoEdge) (NodeListCons head tail))
            )
        )
        SkolemListNil
        acc
    )
    (AudioUnitRef first)
createTest4 =
  create
    $ Gain 1.0 \(gain :: Proxy MyGain) ->
        gain /\ Highpass 330.0 1.0 (SinOsc 440.0) /\ unit

data MySinOsc

createTest5 ::
  forall first mid0 mid1 last env head tail acc proof.
  Succ first mid0 =>
  Succ mid0 mid1 =>
  Succ mid1 last =>
  Frame env proof (UniverseC first (GraphC head tail) SkolemListNil acc)
    ( UniverseC last
        ( GraphC
            ( NodeC (TGain first)
                (ManyEdges first (PtrListCons mid0 (PtrListCons mid1 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass mid0) (SingleEdge mid1))
                (NodeListCons (NodeC (TSinOsc mid1) NoEdge) (NodeListCons head tail))
            )
        )
        SkolemListNil
        acc
    )
    (AudioUnitRef mid1)
createTest5 =
  create
    $ Dup (SinOsc 440.0) \(mySinOsc :: Proxy MySinOsc) ->
        Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ Highpass 330.0 1.0 mySinOsc /\ mySinOsc /\ unit

--
{-
opsTest0 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil Unit)
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        SkolemListNil
        Unit
    )
    (AudioUnitRef D0)
opsTest0 =
  create
    $ Dup (SinOsc 440.0) \(mySinOsc :: Proxy MySinOsc) ->
        Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ Highpass 330.0 1.0 mySinOsc /\ mySinOsc /\ unit

ot1Type ::
  forall f g h.
  (forall a. a -> f a) ->
  (forall a. a -> g a) ->
  (forall a. a -> h a) ->
  Speaker
    ( Dup (g (SinOsc Number))
        ( Proxy MySinOsc ->
          h (Gain Number
            ( Proxy MyGain ->
              Proxy MyGain /\ (f (Highpass Number Number (Proxy MySinOsc)))
                /\ Proxy MySinOsc
                /\ Unit
            ))
        )
    )
ot1Type f g h =
  Speaker
    $ Dup (g (SinOsc 440.0)) \(mySinOsc :: Proxy MySinOsc) ->
        h (Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ f (Highpass 330.0 1.0 mySinOsc) /\ mySinOsc /\ unit)


opsTest1 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil Unit)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D1))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        SkolemListNil
        Unit
    )
    (AudioUnitRef D0)
opsTest1 = create $ ot1Type Identity Identity Identity

opsTest2 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil Unit)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D1))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        SkolemListNil
        Unit
    )
    (AudioUnitRef D0)
opsTest2 = create $ ot1Type Focus Identity Identity

opsTest3 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil Unit)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D1))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        SkolemListNil
        Unit
    )
    (AudioUnitRef D3)
opsTest3 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  cursor (ot1Type Focus Identity Identity)

opsTest4 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil Unit)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D1))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        SkolemListNil
        Unit
    )
    (AudioUnitRef D1)
opsTest4 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  chpf <- cursor (ot1Type Focus Identity Identity)
  cursor (ot1Type Identity Focus Identity)


opsTest5 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil Unit)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D1))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 PtrListNil))
                )
                ( NodeListCons (NodeC (THighpass D3) NoEdge) NodeListNil)
            )
        )
        SkolemListNil
        Unit
    )
    Unit
opsTest5 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  chpf <- cursor (ot1Type Focus Identity Identity)
  csin <- cursor (ot1Type Identity Focus Identity)
  cgain <- cursor (ot1Type Identity Identity Focus)
  disconnect csin chpf
  disconnect csin cgain
  destroy csin
-}
type Time
  = { time :: Number }

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "simple scene" do
          let
            scene0 =
              Speaker
                ( Gain 1.0 \(gain :: Proxy MyGain) ->
                    gain
                      /\ Highpass
                          ( 330.0
                              /\ \({ time } :: Time) (_ :: Unit) (_ :: AudioParameter) ->
                                  330.0 + time * 10.0
                          )
                          1.0
                          (SinOsc 440.0)
                      /\ unit
                )
          it "is coherent" do
            let
              simpleScene =
                start unit
                  (ivoid $ create scene0)
                  loop

              (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame simpleScene { time: 0.0 }

              (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame frame1 { time: 0.1 }

              (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _) = oneFrame frame2 { time: 0.2 }

              nodeAssertion = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param 330.0) (param 1.0)), 3 /\ (ASinOsc (param 440.0)) ]

              edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

              instructionAssertion = A.sortBy testCompare [ NewUnit 1 "gain", NewUnit 2 "highpass", NewUnit 3 "sinosc", ConnectXToY 1 0, ConnectXToY 1 1, ConnectXToY 2 1, ConnectXToY 3 2, SetGain 1 1.0 0.0 LinearRamp, SetFrequency 3 440.0 0.0 LinearRamp, SetFrequency 2 330.0 0.0 LinearRamp, SetQ 2 1.0 0.0 LinearRamp ]
            frame0Nodes `shouldEqual` nodeAssertion
            frame1Nodes `shouldEqual` nodeAssertion
            frame2Nodes `shouldEqual` nodeAssertion
            frame0Edges `shouldEqual` edgeAssertion
            frame1Edges `shouldEqual` edgeAssertion
            frame2Edges `shouldEqual` edgeAssertion
            A.sortBy testCompare frame0Instr `shouldEqual` instructionAssertion
            A.sortBy testCompare frame1Instr `shouldEqual` []
            A.sortBy testCompare frame2Instr `shouldEqual` []
            pure unit
          it "is coherent after change" do
            let
              simpleScene = start unit (ivoid $ create scene0) (makeChangingSceneLoop scene0)

              (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame simpleScene { time: 0.0 }

              (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame frame1 { time: 0.1 }

              (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _) = oneFrame frame2 { time: 0.2 }

              nodeAssertion i = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param $ 330.0 + i) (param 1.0)), 3 /\ (ASinOsc (param 440.0)) ]

              edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

              instructionAssertion = A.sortBy testCompare [ NewUnit 1 "gain", NewUnit 2 "highpass", NewUnit 3 "sinosc", ConnectXToY 1 0, ConnectXToY 1 1, ConnectXToY 2 1, ConnectXToY 3 2, SetGain 1 1.0 0.0 LinearRamp, SetFrequency 3 440.0 0.0 LinearRamp, SetFrequency 2 330.0 0.0 LinearRamp, SetQ 2 1.0 0.0 LinearRamp ]
            frame0Nodes `shouldEqual` (nodeAssertion 0.0)
            frame1Nodes `shouldEqual` (nodeAssertion 1.0)
            frame2Nodes `shouldEqual` (nodeAssertion 2.0)
            frame0Edges `shouldEqual` edgeAssertion
            frame1Edges `shouldEqual` edgeAssertion
            frame2Edges `shouldEqual` edgeAssertion
            A.sortBy testCompare frame0Instr `shouldEqual` instructionAssertion
            A.sortBy testCompare frame1Instr `shouldEqual` [ SetFrequency 2 331.0 0.0 LinearRamp ]
            A.sortBy testCompare frame2Instr `shouldEqual` [ SetFrequency 2 332.0 0.0 LinearRamp ]
            pure unit
