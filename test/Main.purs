module Test.Main where

import Prelude

import Control.Applicative.Indexed (imap, ipure, ivoid)
import Control.Monad.Indexed.Qualified as Ix
import Data.Array as A
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Set as S
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy)
import WAGS (AnAudioUnit(..), AudioParameter, AudioParameterTransition(..), Focus(..), Gain(..), Highpass(..), Instruction(..), SinOsc(..), Speaker(..), branch, change, changeAt, create, cursor, env, freeze, loop, oneFrame, param, start, (@>))

testCompare :: Instruction -> Instruction -> Ordering
testCompare a b = case compare a b of
  EQ -> compare (show a) (show b)
  x -> x

data MyGain

data MySinOsc

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
                  imap Right $ create (scene0 e)
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
                  imap Right $ ivoid $ create (scene0 e)
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
