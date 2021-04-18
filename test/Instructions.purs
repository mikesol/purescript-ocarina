module Test.Instructions where

import Prelude

import Control.Applicative.Indexed (imap)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Set as S
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy)
import WAGS.Change (change, changeAt)
import WAGS.Control.Functions (branch, env, freeze, loop, proof, start, withProof, (@>), (@|>))
import WAGS.Control.Qualified as Ix
import WAGS.Control.Types (oneFrame')
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Graph.Constructors (OnOff(..), SinOsc(..))
import WAGS.Graph.Decorators (Focus(..))
import WAGS.Graph.Optionals (gain, highpass, mix, sinOsc, speaker)
import WAGS.Graph.Parameter (param)
import WAGS.Rendered (AnAudioUnit(..), Instruction(..))

data MyMix

data MyGain

data MySinOsc

type Time
  = { time :: (Number) }

scene0_ f ({ time: time' } :: Time) =
  speaker
    $ mix \(myMix :: Proxy MyMix) ->
        myMix /\ highpass (330.0 + time' * 10.0) (f (sinOsc 440.0)) /\ unit

scene0 = scene0_ Identity

scene1 ({ time: time' } :: Time) =
  speaker
    $ gain 1.0 \(myGain :: Proxy MyGain) ->
        myGain /\ highpass (330.0 + time' * 50.0) (sinOsc 440.0) /\ unit

resolveInstructions :: Array (Unit -> Instruction) -> Array Instruction
resolveInstructions = map (_ $ unit)

testInstructions :: Spec Unit
testInstructions = do
  describe "a simple scene that doesn't change" do
    let
      simpleScene =
        ( Ix.do
            start
            e <- env
            create (scene0 e)
        )
          @|> freeze

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _) = oneFrame' frame2 { time: 0.2 }

      nodeAssertion = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param 330.0) (param 1.0)), 3 /\ (ASinOsc On (param 440.0)) ]

      edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

      instructionAssertion =
        [ (MakeSpeaker 0)
        , MakeGain 1 (param 1.0)
        , MakeHighpass 2 (param 330.0) (param 1.0)
        , MakeSinOsc 3 On (param 440.0)
        , (ConnectXToY 3 2)
        , (ConnectXToY 1 1)
        , (ConnectXToY 2 1)
        , (ConnectXToY 1 0)
        ]
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
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "is coherent at frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` []
    it "is coherent at frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` []
  describe "a simple scene that changes only the sine wave osc as a function of time" do
    let
      simpleScene =
        ( Ix.do
            start
            e <- env
            imap Right $ ivoid $ create (scene0 e)
        )
          @> ( loop
                ( const
                    $ Ix.do
                        e <- env
                        sosc <- cursor (scene0_ Focus e)
                        ivoid $ changeAt sosc (SinOsc On $ 440.0 + e.time * 50.0)
                )
            )

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Nodes /\ frame3Edges /\ frame3Instr /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Nodes /\ frame4Edges /\ frame4Instr /\ _) = oneFrame' frame4 { time: 0.4 }

      nodeAssertion i = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param $ 330.0) (param 1.0)), 3 /\ (ASinOsc On (param $ 440.0 + i)) ]

      edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

      instructionAssertion =
        [ (MakeSpeaker 0)
        , MakeGain 1 (param 1.0)
        , MakeHighpass 2 (param 330.0) (param 1.0)
        , MakeSinOsc 3 On (param 440.0)
        , (ConnectXToY 3 2)
        , (ConnectXToY 1 1)
        , (ConnectXToY 2 1)
        , (ConnectXToY 1 0)
        ]
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
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "is coherent after frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency 3 $ param 445.0 ]
    it "is coherent after frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency 3 $ param 450.0 ]
  describe "a simple scene that changes the entire graph as a function of time" do
    let
      simpleScene =
        ( Ix.do
            start
            e <- env
            create (scene0 e) $> Right unit
        )
          @> ( loop
                ( const
                    $ Ix.do
                        e <- env
                        ivoid $ change (scene0 e)
                )
            )

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Nodes /\ frame3Edges /\ frame3Instr /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Nodes /\ frame4Edges /\ frame4Instr /\ _) = oneFrame' frame4 { time: 0.4 }

      nodeAssertion i = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param $ 330.0 + i) (param 1.0)), 3 /\ (ASinOsc On (param 440.0)) ]

      edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

      instructionAssertion =
        [ (MakeSpeaker 0)
        , MakeGain 1 (param 1.0)
        , MakeHighpass 2 (param 330.0) (param 1.0)
        , MakeSinOsc 3 On (param 440.0)
        , (ConnectXToY 3 2)
        , (ConnectXToY 1 1)
        , (ConnectXToY 2 1)
        , (ConnectXToY 1 0)
        ]
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
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "is coherent after frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency 2 $ param 331.0 ]
    it "is coherent after frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency 2 $ param 332.0 ]
  describe "a scene that forks at 0.3 seconds" do
    let
      simpleScene =
        ( Ix.do
            start
            e <- env
            create (scene0 e) $> Right unit
        )
          @> ( branch Ix.do
                { time } <- env
                pr <- proof
                withProof pr
                  $ if time < 0.3 then
                      Right
                        ( const
                            $ Ix.do
                                e <- env
                                ivoid $ change (scene0 e)
                        )
                    else
                      Left
                        ( loop
                            ( const
                                $ Ix.do
                                    e <- env
                                    ivoid $ change (scene1 e)
                            )
                        )
            )

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Nodes /\ frame3Edges /\ frame3Instr /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Nodes /\ frame4Edges /\ frame4Instr /\ _) = oneFrame' frame4 { time: 0.4 }

      nodeAssertion i = M.fromFoldable [ 0 /\ ASpeaker, 1 /\ (AGain (param 1.0)), 2 /\ (AHighpass (param $ 330.0 + i) (param 1.0)), 3 /\ (ASinOsc On (param 440.0)) ]

      edgeAssertion = M.fromFoldable [ 0 /\ S.singleton 1, 1 /\ S.fromFoldable [ 1, 2 ], 2 /\ S.singleton 3 ]

      instructionAssertion =
        [ (MakeSpeaker 0)
        , MakeGain 1 (param 1.0)
        , MakeHighpass 2 (param 330.0) (param 1.0)
        , MakeSinOsc 3 On (param 440.0)
        , (ConnectXToY 3 2)
        , (ConnectXToY 1 1)
        , (ConnectXToY 2 1)
        , (ConnectXToY 1 0)
        ]
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
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "branches at frame0Instr" do
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency 2 $ param 331.0 ]
    it "branches at frame1Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency 2 $ param 332.0 ]
    it "branches at frame2Instr" do
      resolveInstructions frame3Instr `shouldEqual` [ SetFrequency 2 $ param 345.0 ]
    it "branches at frame3Instr" do
      resolveInstructions frame4Instr `shouldEqual` [ SetFrequency 2 $ param 350.0 ]
