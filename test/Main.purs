module Test.Main where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Array as A
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Set as S
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Bool (True, False)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))
import WAGS (class AllEdgesPointToNodes, class AudioUnitEq, class BinEq, class BinSub, class BinSucc, class BinToInt, class Gate, class HasBottomLevelNodes, class Lookup, class NoNodesAreDuplicated, class NoParallelEdges, class UniqueTerminus, type (+:), type (/->), type (/:), AnAudioUnit(..), AudioParameter, AudioParameterTransition(..), AudioUnitRef, Bc, Bn, D0, D1, D2, D3, D4, Dup(..), Focus(..), Frame, Gain(..), GraphC, Highpass(..), I, InitialGraph, Instruction(..), ManyEdges, NoEdge, NodeC, NodeListCons, NodeListNil, O, PtrListCons, PtrListNil, SinOsc(..), SingleEdge, SkolemListNil, Speaker(..), TGain, THighpass, TSinOsc, TSpeaker, UniverseC, branch, change, changeAt, create, cursor, destroy, disconnect, env, freeze, loop, oneFrame, param, start, testCompare, (@>))

---- data
data MyGain

data MySinOsc

---------------------------
----- tests
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
  forall ptr next env skolems head tail proof.
  Semigroup env =>
  BinToInt ptr =>
  BinToInt next =>
  BinSucc ptr next =>
  Frame env proof (UniverseC ptr (GraphC head tail) skolems)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr) NoEdge) (NodeListCons head tail))
        skolems
    )
    (AudioUnitRef ptr)
createTest1 = create (SinOsc 440.0)

createTest2 ::
  forall first mid last env skolems head tail proof.
  Semigroup env =>
  BinToInt first =>
  BinToInt mid =>
  BinToInt last =>
  BinSucc first mid =>
  BinSucc mid last =>
  Frame env proof (UniverseC first (GraphC head tail) skolems)
    ( UniverseC last
        (GraphC (NodeC (THighpass first) (SingleEdge mid)) (NodeListCons (NodeC (TSinOsc mid) NoEdge) (NodeListCons head tail)))
        skolems
    )
    (AudioUnitRef first)
createTest2 = create (Highpass 440.0 1.0 (SinOsc 440.0))

createTest3 ::
  forall first mid last env head tail proof.
  Semigroup env =>
  BinToInt first =>
  BinToInt mid =>
  BinToInt last =>
  BinSucc first mid =>
  BinSucc mid last =>
  Frame env proof (UniverseC first (GraphC head tail) SkolemListNil)
    ( UniverseC last
        ( GraphC
            ( NodeC (TGain first)
                (ManyEdges first (PtrListCons mid PtrListNil))
            )
            (NodeListCons (NodeC (TSinOsc mid) NoEdge) (NodeListCons head tail))
        )
        SkolemListNil
    )
    (AudioUnitRef first)
createTest3 = create (Gain 1.0 (\(gain :: Proxy MyGain) -> gain /\ SinOsc 440.0 /\ unit))

createTest4 ::
  forall first mid0 mid1 last env head tail proof.
  Semigroup env =>
  BinToInt first =>
  BinToInt mid0 =>
  BinToInt mid1 =>
  BinToInt last =>
  BinSucc first mid0 =>
  BinSucc mid0 mid1 =>
  BinSucc mid1 last =>
  Frame env proof (UniverseC first (GraphC head tail) SkolemListNil)
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
    )
    (AudioUnitRef first)
createTest4 =
  create
    $ Gain 1.0 \(gain :: Proxy MyGain) ->
        gain /\ Highpass 330.0 1.0 (SinOsc 440.0) /\ unit

createTest5 ::
  forall env head tail proof r g.
  Semigroup env =>
  Frame env proof (UniverseC D0 (GraphC head tail) SkolemListNil)
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) (NodeListCons head tail))
            )
        )
        SkolemListNil
    )
    (AudioUnitRef D1)
createTest5 =
  create
    $ Dup (SinOsc 440.0) \(mySinOsc :: Proxy MySinOsc) ->
        Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ Highpass 330.0 1.0 mySinOsc /\ mySinOsc /\ unit

--
opsTest0 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
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
    )
    (AudioUnitRef D1)
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
          h
            ( Gain Number
                ( Proxy MyGain ->
                  Proxy MyGain /\ (f (Highpass Number Number (Proxy MySinOsc)))
                    /\ Proxy MySinOsc
                    /\ Unit
                )
            )
        )
    )
ot1Type f g h =
  Speaker
    $ Dup (g (SinOsc 440.0)) \(mySinOsc :: Proxy MySinOsc) ->
        h
          ( Gain 1.0 \(gain :: Proxy MyGain) ->
              gain /\ f (Highpass 330.0 1.0 mySinOsc) /\ mySinOsc /\ unit
          )

opsTest1 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
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
    )
    (AudioUnitRef D0)
opsTest1 = create $ ot1Type Identity Identity Identity

opsTest2 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
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
    )
    (AudioUnitRef D0)
opsTest2 = create $ ot1Type Focus Identity Identity

opsTest3 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
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
    )
    (AudioUnitRef D3)
opsTest3 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  cursor (ot1Type Focus Identity Identity)

opsTest4 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
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
    )
    (AudioUnitRef D1)
opsTest4 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  chpf <- cursor (ot1Type Focus Identity Identity)
  cursor (ot1Type Identity Focus Identity)

opsTest5 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
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
    )
    (AudioUnitRef D2)
opsTest5 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  chpf <- cursor (ot1Type Focus Identity Identity)
  csin <- cursor (ot1Type Identity Focus Identity)
  cursor (ot1Type Identity Identity Focus)

opsTest6 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) NoEdge)
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        SkolemListNil
    )
    Unit
opsTest6 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  chpf <- cursor (ot1Type Focus Identity Identity)
  csin <- cursor (ot1Type Identity Focus Identity)
  cgain <- cursor (ot1Type Identity Identity Focus)
  disconnect csin chpf

opsTest7 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D1 PtrListNil))
                )
                ( NodeListCons (NodeC (THighpass D3) NoEdge)
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        SkolemListNil
    )
    Unit
opsTest7 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  chpf <- cursor (ot1Type Focus Identity Identity)
  csin <- cursor (ot1Type Identity Focus Identity)
  cgain <- cursor (ot1Type Identity Identity Focus)
  disconnect csin chpf
  disconnect chpf cgain

opsTest8 ::
  Frame Unit Void (UniverseC D0 InitialGraph SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D1 PtrListNil))
                )
                (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
            )
        )
        SkolemListNil
    )
    Unit
opsTest8 = Ix.do
  ivoid $ create $ ot1Type Identity Identity Identity
  chpf <- cursor (ot1Type Focus Identity Identity)
  csin <- cursor (ot1Type Identity Focus Identity)
  cgain <- cursor (ot1Type Identity Identity Focus)
  disconnect csin chpf
  disconnect chpf cgain
  destroy chpf

type Time
  = { time :: (Number) }

scene0_ f ({ time: time' } :: Time) =
  Speaker
    ( Gain 1.0 \(gain :: Proxy MyGain) ->
        gain
          /\ Highpass
              ( 330.0
                  /\ \(_ :: AudioParameter) ->
                      330.0 + time' * 10.0
              )
              1.0
              (f (SinOsc 440.0))
          /\ unit
    )

scene0 = scene0_ Identity

scene1 ({ time: time' } :: Time) =
  Speaker
    ( Gain 1.0 \(gain :: Proxy MyGain) ->
        gain
          /\ Highpass
              ( 330.0
                  /\ \(_ :: AudioParameter) ->
                      330.0 + time' * 50.0
              )
              1.0
              (SinOsc 440.0)
          /\ unit
    )

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "a simple scene that doesn't change" do
          let
            simpleScene =
              ( Ix.do
                  start
                  e <- env
                  ivoid $ create (scene0 e)
                  ipure $ Right unit
              )
                @> freeze

            (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame simpleScene { time: 0.0 }

            (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame frame1 { time: 0.1 }

            (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _) = oneFrame frame2 { time: 0.2 }

            nodeAssertion = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param 330.0) (param 1.0)), 3 /\ (ASinOsc (param 440.0)) ]

            edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

            instructionAssertion = A.sortBy testCompare [ NewUnit 1 "gain", NewUnit 2 "highpass", NewUnit 3 "sinosc", ConnectXToY 1 0, ConnectXToY 1 1, ConnectXToY 2 1, ConnectXToY 3 2, SetGain 1 1.0 0.0 LinearRamp, SetFrequency 3 440.0 0.0 LinearRamp, SetFrequency 2 330.0 0.0 LinearRamp, SetQ 2 1.0 0.0 LinearRamp ]
          it "is coherent at frame0Nodes" do
            frame0Nodes `shouldEqual` nodeAssertion
          it "is coherent at frame1Nodes" do
            frame1Nodes `shouldEqual` nodeAssertion
          it "is coherent at frame2Nodes" do
            frame2Nodes `shouldEqual` nodeAssertion
          it "is coherent at frame0Edges" do
            frame0Edges `shouldEqual` edgeAssertion
          it "is coherent at frame1Edges" do
            frame1Edges `shouldEqual` edgeAssertion
          it "is coherent at frame2Edges" do
            frame2Edges `shouldEqual` edgeAssertion
          it "is coherent at frame0Instr" do
            A.sortBy testCompare frame0Instr `shouldEqual` instructionAssertion
          it "is coherent at frame1Instr" do
            A.sortBy testCompare frame1Instr `shouldEqual` []
          it "is coherent at frame0Instr" do
            A.sortBy testCompare frame2Instr `shouldEqual` []
        describe "a simple scene that changes only the sine wave osc as a function of time" do
          let
            simpleScene =
              ( Ix.do
                  start
                  e <- env
                  ivoid $ create (scene0 e)
                  ipure $ Right unit
              )
                @> ( loop
                      ( const
                          $ Ix.do
                              e <- env
                              sosc <- cursor (scene0_ Focus e)
                              ivoid $ changeAt sosc (SinOsc $ 440.0 + e.time * 50.0)
                      )
                  )

            (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame simpleScene { time: 0.0 }

            (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame frame1 { time: 0.1 }

            (frame2Nodes /\ frame2Edges /\ frame2Instr /\ frame3) = oneFrame frame2 { time: 0.2 }

            (frame3Nodes /\ frame3Edges /\ frame3Instr /\ frame4) = oneFrame frame3 { time: 0.3 }

            (frame4Nodes /\ frame4Edges /\ frame4Instr /\ _) = oneFrame frame4 { time: 0.4 }

            nodeAssertion i = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param $ 330.0) (param 1.0)), 3 /\ (ASinOsc (param $ 440.0 + i)) ]

            edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

            instructionAssertion = A.sortBy testCompare [ NewUnit 1 "gain", NewUnit 2 "highpass", NewUnit 3 "sinosc", ConnectXToY 1 0, ConnectXToY 1 1, ConnectXToY 2 1, ConnectXToY 3 2, SetGain 1 1.0 0.0 LinearRamp, SetFrequency 3 440.0 0.0 LinearRamp, SetFrequency 2 330.0 0.0 LinearRamp, SetQ 2 1.0 0.0 LinearRamp ]
          it "is coherent after frame0Nodes" do
            frame0Nodes `shouldEqual` (nodeAssertion 0.0)
          it "is coherent after frame1Nodes" do
            frame1Nodes `shouldEqual` (nodeAssertion 5.0)
          it "is coherent after frame2Nodes" do
            frame2Nodes `shouldEqual` (nodeAssertion 10.0)
          it "is coherent after frame3Nodes" do
            frame3Nodes `shouldEqual` (nodeAssertion 15.0)
          it "is coherent after frame4Nodes" do
            frame4Nodes `shouldEqual` (nodeAssertion 20.0)
          it "is coherent after frame0Edges" do
            frame0Edges `shouldEqual` edgeAssertion
          it "is coherent after frame1Edges" do
            frame1Edges `shouldEqual` edgeAssertion
          it "is coherent after frame2Edges" do
            frame2Edges `shouldEqual` edgeAssertion
          it "is coherent after frame0Instr" do
            A.sortBy testCompare frame0Instr `shouldEqual` instructionAssertion
          it "is coherent after frame1Instr" do
            A.sortBy testCompare frame1Instr `shouldEqual` [ SetFrequency 3 445.0 0.0 LinearRamp ]
          it "is coherent after frame2Instr" do
            A.sortBy testCompare frame2Instr `shouldEqual` [ SetFrequency 3 450.0 0.0 LinearRamp ]
        describe "a simple scene that changes the entire graph as a function of time" do
          let
            simpleScene =
              ( Ix.do
                  start
                  e <- env
                  ivoid $ create (scene0 e)
                  ipure $ Right unit
              )
                @> ( loop
                      ( const
                          $ Ix.do
                              e <- env
                              ivoid $ change (scene0 e)
                      )
                  )

            (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame simpleScene { time: 0.0 }

            (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame frame1 { time: 0.1 }

            (frame2Nodes /\ frame2Edges /\ frame2Instr /\ frame3) = oneFrame frame2 { time: 0.2 }

            (frame3Nodes /\ frame3Edges /\ frame3Instr /\ frame4) = oneFrame frame3 { time: 0.3 }

            (frame4Nodes /\ frame4Edges /\ frame4Instr /\ _) = oneFrame frame4 { time: 0.4 }

            nodeAssertion i = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param $ 330.0 + i) (param 1.0)), 3 /\ (ASinOsc (param 440.0)) ]

            edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

            instructionAssertion = A.sortBy testCompare [ NewUnit 1 "gain", NewUnit 2 "highpass", NewUnit 3 "sinosc", ConnectXToY 1 0, ConnectXToY 1 1, ConnectXToY 2 1, ConnectXToY 3 2, SetGain 1 1.0 0.0 LinearRamp, SetFrequency 3 440.0 0.0 LinearRamp, SetFrequency 2 330.0 0.0 LinearRamp, SetQ 2 1.0 0.0 LinearRamp ]
          it "is coherent after frame0Nodes" do
            frame0Nodes `shouldEqual` (nodeAssertion 0.0)
          it "is coherent after frame1Nodes" do
            frame1Nodes `shouldEqual` (nodeAssertion 1.0)
          it "is coherent after frame2Nodes" do
            frame2Nodes `shouldEqual` (nodeAssertion 2.0)
          it "is coherent after frame3Nodes" do
            frame3Nodes `shouldEqual` (nodeAssertion 3.0)
          it "is coherent after frame4Nodes" do
            frame4Nodes `shouldEqual` (nodeAssertion 4.0)
          it "is coherent after frame0Edges" do
            frame0Edges `shouldEqual` edgeAssertion
          it "is coherent after frame1Edges" do
            frame1Edges `shouldEqual` edgeAssertion
          it "is coherent after frame2Edges" do
            frame2Edges `shouldEqual` edgeAssertion
          it "is coherent after frame0Instr" do
            A.sortBy testCompare frame0Instr `shouldEqual` instructionAssertion
          it "is coherent after frame1Instr" do
            A.sortBy testCompare frame1Instr `shouldEqual` [ SetFrequency 2 331.0 0.0 LinearRamp ]
          it "is coherent after frame2Instr" do
            A.sortBy testCompare frame2Instr `shouldEqual` [ SetFrequency 2 332.0 0.0 LinearRamp ]
        describe "a scene that forks at 0.3 seconds" do
          let
            simpleScene =
              ( Ix.do
                  start
                  e <- env
                  ivoid $ create (scene0 e)
                  ipure $ Right unit
              )
                @> ( let
                      cont =
                        loop
                          ( const
                              $ Ix.do
                                  e <- env
                                  ivoid $ change (scene1 e)
                          )

                      hold =
                        const
                          $ Ix.do
                              e <- env
                              ivoid $ change (scene0 e)

                      split = Ix.do
                        { time } <- env
                        ipure $ if time < 0.3 then Right hold else Left cont
                    in
                      branch split
                  )

            (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame simpleScene { time: 0.0 }

            (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame frame1 { time: 0.1 }

            (frame2Nodes /\ frame2Edges /\ frame2Instr /\ frame3) = oneFrame frame2 { time: 0.2 }

            (frame3Nodes /\ frame3Edges /\ frame3Instr /\ frame4) = oneFrame frame3 { time: 0.3 }

            (frame4Nodes /\ frame4Edges /\ frame4Instr /\ _) = oneFrame frame4 { time: 0.4 }

            nodeAssertion i = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param $ 330.0 + i) (param 1.0)), 3 /\ (ASinOsc (param 440.0)) ]

            edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

            instructionAssertion = A.sortBy testCompare [ NewUnit 1 "gain", NewUnit 2 "highpass", NewUnit 3 "sinosc", ConnectXToY 1 0, ConnectXToY 1 1, ConnectXToY 2 1, ConnectXToY 3 2, SetGain 1 1.0 0.0 LinearRamp, SetFrequency 3 440.0 0.0 LinearRamp, SetFrequency 2 330.0 0.0 LinearRamp, SetQ 2 1.0 0.0 LinearRamp ]
          it "branches at frame0Nodes" do
            frame0Nodes `shouldEqual` (nodeAssertion 0.0)
          it "branches at frame1Nodes" do
            frame1Nodes `shouldEqual` (nodeAssertion 1.0)
          it "branches at frame2Nodes" do
            frame2Nodes `shouldEqual` (nodeAssertion 2.0)
          it "branches at frame3Nodes" do
            frame3Nodes `shouldEqual` (nodeAssertion 15.0)
          it "branches at frame4Nodes" do
            frame4Nodes `shouldEqual` (nodeAssertion 20.0)
          it "branches at frame0Edges" do
            frame0Edges `shouldEqual` edgeAssertion
          it "branches at edgeAssertion" do
            frame1Edges `shouldEqual` edgeAssertion
          it "branches at edgeAssertion" do
            frame2Edges `shouldEqual` edgeAssertion
          it "branches at edgeAssertion" do
            A.sortBy testCompare frame0Instr `shouldEqual` instructionAssertion
          it "branches at frame0Instr" do
            A.sortBy testCompare frame1Instr `shouldEqual` [ SetFrequency 2 331.0 0.0 LinearRamp ]
          it "branches at frame1Instr" do
            A.sortBy testCompare frame2Instr `shouldEqual` [ SetFrequency 2 332.0 0.0 LinearRamp ]
          it "branches at frame2Instr" do
            A.sortBy testCompare frame3Instr `shouldEqual` [ SetFrequency 2 345.0 0.0 LinearRamp ]
          it "branches at frame3Instr" do
            A.sortBy testCompare frame4Instr `shouldEqual` [ SetFrequency 2 350.0 0.0 LinearRamp ]
