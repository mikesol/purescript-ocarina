module Test.Instructions where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Map as M
import Data.Set as S
import Data.Tuple.Nested ((/\), type (/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, env, freeze, loop, proof, start, withProof, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame, Frame0, InitialGraph, oneFrame')
import WAGS.Create (create)
import WAGS.Get (get)
import WAGS.Graph.AudioUnit (Gain, Highpass, OnOff(..), SinOsc, Speaker, TGain, THighpass, TSinOsc, TSpeaker, GetSinOsc)
import WAGS.Graph.Optionals (GetSetAP, Ref, gain, highpass, highpass_, ref, sinOsc, sinOsc_, speaker)
import WAGS.Graph.Parameter ( param)
import WAGS.Rendered (AnAudioUnit(..), Instruction(..))

type Time
  = { time :: Number }

type SceneTemplate
  = { speaker ∷
        Speaker
          /\ { mix ::
              Gain GetSetAP
                /\ { highpass ::
                    Highpass GetSetAP GetSetAP
                      /\ { sinOsc :: SinOsc GetSetAP /\ {}
                      }
                , mix :: Ref
                }
          }
    }

type SceneType
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { mix :: Unit, highpass :: Unit }
    , highpass :: THighpass /\ { sinOsc :: Unit }
    , sinOsc :: TSinOsc /\ {}
    }

scene0 :: Time -> SceneTemplate
scene0 { time: time' } =
  speaker
    { mix:
        gain 1.0
          { mix: ref
          , highpass:
              highpass (330.0 + time' * 10.0)
                { sinOsc: sinOsc 440.0 }
          }
    }

scene1 :: Time -> SceneTemplate
scene1 ({ time: time' } :: Time) =
  speaker
    { mix:
        gain 1.0
          { mix: ref
          , highpass:
              highpass (330.0 + time' * 50.0)
                { sinOsc: sinOsc 440.0 }
          }
    }

resolveInstructions :: Array (Unit -> Instruction) -> Array Instruction
resolveInstructions = map (_ $ unit)

testInstructions :: Spec Unit
testInstructions = do
  describe "a simple scene that doesn't change" do
    let
      simpleFrame :: Frame Time Unit Instruction Frame0 InitialGraph SceneType Unit
      simpleFrame = WAGS.do
        start
        e <- env
        create (scene0 e)

      simpleScene = simpleFrame @|> freeze

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _ /\ _) = oneFrame' frame2 { time: 0.2 }

      nodeAssertion = M.fromFoldable [ "speaker" /\ ASpeaker, "mix" /\ (AGain (param 1.0)), "highpass" /\ (AHighpass (param 330.0) (param 1.0)), "sinOsc" /\ (ASinOsc On (param 440.0)) ]

      edgeAssertion = M.fromFoldable [ "speaker" /\ S.singleton "mix", "mix" /\ S.fromFoldable [ "mix", "highpass" ], "highpass" /\ S.singleton "sinOsc" ]

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (param 1.0)
        , MakeHighpass "highpass" (param 330.0) (param 1.0)
        , MakeSinOsc "sinOsc" On (param 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
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
      simpleFrame :: Frame Time Unit Instruction Frame0 InitialGraph SceneType Unit
      simpleFrame = WAGS.do
        start
        e <- env
        create (scene0 e)

      simpleScene =
        simpleFrame
          @|> ( loop
                ( const
                    $ WAGS.do
                        e <- env
                        ivoid
                          $ change
                              { sinOsc: sinOsc_ (440.0 + e.time * 50.0) }
                )
            )

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _ /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Nodes /\ _ /\ _ /\ _ /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Nodes /\ _ /\ _ /\ _ /\ _) = oneFrame' frame4 { time: 0.4 }

      nodeAssertion i = M.fromFoldable [ "speaker" /\ ASpeaker, "mix" /\ (AGain (param 1.0)), "highpass" /\ (AHighpass (param $ 330.0) (param 1.0)), "sinOsc" /\ (ASinOsc On (param $ 440.0 + i)) ]

      edgeAssertion = M.fromFoldable [ "speaker" /\ S.singleton "mix", "mix" /\ S.fromFoldable [ "mix", "highpass" ], "highpass" /\ S.singleton "sinOsc" ]

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (param 1.0)
        , MakeHighpass "highpass" (param 330.0) (param 1.0)
        , MakeSinOsc "sinOsc" On (param 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
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
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency "sinOsc" $ param 445.0 ]
    it "is coherent after frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency "sinOsc" $ param 450.0 ]
  describe "a simple scene that changes several elements as a function of time" do
    let
      simpleFrame :: Frame Time Unit Instruction Frame0 InitialGraph SceneType Unit
      simpleFrame = WAGS.do
        start
        e <- env
        create (scene0 e)

      simpleScene =
        simpleFrame
          @|> ( loop
                ( const
                    $ WAGS.do
                        e <- env
                        ivoid
                          $ change
                              { sinOsc: sinOsc_ (440.0 + e.time * 50.0)
                              , highpass: highpass_ (330.0 + e.time * 10.0)
                              }
                )
            )

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _ /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Nodes /\ _ /\ _ /\ _ /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Nodes /\ _ /\ _ /\ _ /\ _) = oneFrame' frame4 { time: 0.4 }

      nodeAssertion i = M.fromFoldable [ "speaker" /\ ASpeaker, "mix" /\ (AGain (param 1.0)), "highpass" /\ (AHighpass (param $ 330.0 + i) (param 1.0)), "sinOsc" /\ (ASinOsc On (param $ 440.0 + i * 5.0)) ]

      edgeAssertion = M.fromFoldable [ "speaker" /\ S.singleton "mix", "mix" /\ S.fromFoldable [ "mix", "highpass" ], "highpass" /\ S.singleton "sinOsc" ]

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (param 1.0)
        , MakeHighpass "highpass" (param 330.0) (param 1.0)
        , MakeSinOsc "sinOsc" On (param 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
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
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency "highpass" $ param 331.0, SetFrequency "sinOsc" $ param 445.0 ]
    it "is coherent after frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency "highpass" $ param 332.0, SetFrequency "sinOsc" $ param 450.0 ]
  describe "a simple scene that gets and then sets with the getter" do
    let
      simpleFrame :: Frame Time Unit Instruction Frame0 InitialGraph SceneType Unit
      simpleFrame = WAGS.do
        start
        e <- env
        create (scene0 e)

      simpleScene =
        simpleFrame
          @|> ( loop
                ( const
                    $ WAGS.do
                        e <- env
                        -- todo: improve get mechanism
                        gotten :: { sinOsc :: GetSinOsc } <- get { sinOsc: unit }
                        ivoid $ change gotten
                )
            )

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _ /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Nodes /\ _ /\ _ /\ _ /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Nodes /\ _ /\ _ /\ _ /\ _) = oneFrame' frame4 { time: 0.4 }

      nodeAssertion i = M.fromFoldable [ "speaker" /\ ASpeaker, "mix" /\ (AGain (param 1.0)), "highpass" /\ (AHighpass (param $ 330.0 + i) (param 1.0)), "sinOsc" /\ (ASinOsc On (param 440.0)) ]

      edgeAssertion = M.fromFoldable [ "speaker" /\ S.singleton "mix", "mix" /\ S.fromFoldable [ "mix", "highpass" ], "highpass" /\ S.singleton "sinOsc" ]

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (param 1.0)
        , MakeHighpass "highpass" (param 330.0) (param 1.0)
        , MakeSinOsc "sinOsc" On (param 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
        ]
    it "is coherent after frame0Nodes" do
      frame0Nodes `shouldEqual` (nodeAssertion 0.0)
    it "is coherent after frame1Nodes" do
      frame1Nodes `shouldEqual` (nodeAssertion 0.0)
    it "is coherent after frame2Nodes" do
      frame2Nodes `shouldEqual` (nodeAssertion 0.0)
    it "is coherent after frame3Nodes" do
      frame3Nodes `shouldEqual` (nodeAssertion 0.0)
    it "is coherent after frame4Nodes" do
      frame4Nodes `shouldEqual` (nodeAssertion 0.0)
    it "is coherent after frame0Edges" do
      frame0Edges `shouldEqual` edgeAssertion
    it "is coherent after frame1Edges" do
      frame1Edges `shouldEqual` edgeAssertion
    it "is coherent after frame2Edges" do
      frame2Edges `shouldEqual` edgeAssertion
    it "is coherent after frame0Instr" do
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "is coherent after frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` []
    it "is coherent after frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` []
  describe "a scene that forks at 0.3 seconds" do
    let
      simpleFrame :: Frame Time Unit Instruction Frame0 InitialGraph SceneType Unit
      simpleFrame = WAGS.do
        start
        e <- env
        create (scene0 e)

      simpleScene =
        simpleFrame
          @|> ( branch \_ -> WAGS.do
                { time } <- env
                pr <- proof
                withProof pr
                  $ if time < 0.3 then
                      Right
                        ( WAGS.do
                            e <- env
                            ivoid
                              $ change
                                  { highpass: highpass_ (330.0 + e.time * 10.0)
                                  }
                        )
                    else
                      Left
                        ( loop
                            ( const
                                $ WAGS.do
                                    e <- env
                                    ivoid
                                      $ change
                                          { highpass: highpass_ (330.0 + e.time * 50.0)
                                          }
                            )
                        )
            )

      (frame0Nodes /\ frame0Edges /\ frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Nodes /\ frame1Edges /\ frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Nodes /\ frame2Edges /\ frame2Instr /\ _ /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Nodes /\ _ /\ frame3Instr /\ _ /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Nodes /\ _ /\ frame4Instr /\ _ /\ _) = oneFrame' frame4 { time: 0.4 }

      nodeAssertion i = M.fromFoldable [ "speaker" /\ ASpeaker, "mix" /\ (AGain (param 1.0)), "highpass" /\ (AHighpass (param $ 330.0 + i) (param 1.0)), "sinOsc" /\ (ASinOsc On (param 440.0)) ]

      edgeAssertion = M.fromFoldable [ "speaker" /\ S.singleton "mix", "mix" /\ S.fromFoldable [ "mix", "highpass" ], "highpass" /\ S.singleton "sinOsc" ]

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (param 1.0)
        , MakeHighpass "highpass" (param 330.0) (param 1.0)
        , MakeSinOsc "sinOsc" On (param 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
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
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency "highpass" $ param 331.0 ]
    it "branches at frame1Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency "highpass" $ param 332.0 ]
    it "branches at frame2Instr" do
      resolveInstructions frame3Instr `shouldEqual` [ SetFrequency "highpass" $ param 345.0 ]
    it "branches at frame3Instr" do
      resolveInstructions frame4Instr `shouldEqual` [ SetFrequency "highpass" $ param 350.0 ]
