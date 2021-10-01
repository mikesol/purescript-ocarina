module Test.Instructions where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Change (change, ichange)
import WAGS.Control.Functions (branch, freeze, ibranch, icont, iloop, loop, loopUsingScene, start, (@|>))
import WAGS.Control.Types (Frame0, Scene, WAG, oneFrame')
import WAGS.Create (create)
import WAGS.Create.Optionals (CGain, CHighpass, CSinOsc, CSpeaker, Ref, gain, highpass, ref, sinOsc, speaker)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Rendered (Instruction(..))

type Time
  = { time :: Number }

type SceneTemplate
  = CSpeaker
  { mix ::
      CGain
        { highpass ::
            CHighpass { sinOsc :: CSinOsc }
        , mix :: Ref
        }
  }

type SceneType
  =
  ( speaker :: TSpeaker /\ { mix :: Unit }
  , mix :: TGain /\ { mix :: Unit, highpass :: Unit }
  , highpass :: THighpass /\ { sinOsc :: Unit }
  , sinOsc :: TSinOsc /\ {}
  )

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
      simpleFrame :: Time -> WAG Unit Instruction Frame0 Unit SceneType Unit
      simpleFrame e = create (start $> scene0 e)

      simpleScene = simpleFrame @|> freeze

      (frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Instr /\ _ /\ _) = oneFrame' frame2 { time: 0.2 }

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (pure 1.0)
        , MakeHighpass "highpass" (pure 330.0) (pure 1.0)
        , MakeSinOsc "sinOsc" (pure On) (pure 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
        ]
    it "is coherent at frame0Instr" do
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "is coherent at frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` []
    it "is coherent at frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` []
  describe "a simple scene using a utility function" do
    let
      scene :: Scene Unit Unit Instruction Frame0 Unit
      scene = loopUsingScene
        ( const $ const $
            { control: unit
            , scene: speaker
                { mix:
                    gain 0.7 { sinOsc: sinOsc 441.0 }

                }
            }
        )
        unit
      (frame0Instr /\ _ /\ frame1) = oneFrame' scene unit

      (frame1Instr /\ _ /\ frame2) = oneFrame' frame1 unit

      (frame2Instr /\ _ /\ _) = oneFrame' frame2 unit

      createAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (pure 0.7)
        , MakeSinOsc "sinOsc" (pure On) (pure 441.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "sinOsc" "mix"
        ]
      setAssertion =
        [ (SetGain "mix" (pure 0.7))
        , (SetOnOff "sinOsc" (pure On))
        , (SetFrequency "sinOsc" (pure 441.0))
        ]
    it "is coherent at frame0Instr" do
      resolveInstructions frame0Instr `shouldEqual` createAssertion
    it "is coherent at frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` setAssertion
    it "is coherent at frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` setAssertion
    pure unit
  describe "a simple scene that changes only the sine wave osc as a function of time" do
    let
      simpleFrame :: Time -> WAG Unit Instruction Frame0 Unit SceneType Unit
      simpleFrame e = create (start $> scene0 e)

      simpleScene =
        simpleFrame
          @|> loop \fr -> do
            e <- ask
            pure $ change (fr $> { sinOsc: 440.0 + e.time * 50.0 })

      (frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Instr /\ _ /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (_ /\ _ /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (_ /\ _ /\ _) = oneFrame' frame4 { time: 0.4 }

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (pure 1.0)
        , MakeHighpass "highpass" (pure 330.0) (pure 1.0)
        , MakeSinOsc "sinOsc" (pure On) (pure 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
        ]
    it "is coherent after frame0Instr" do
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "is coherent after frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency "sinOsc" $ pure 445.0 ]
    it "is coherent after frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency "sinOsc" $ pure 450.0 ]
  describe "a simple scene that changes several elements as a function of time" do
    let
      simpleFrame :: Time -> WAG Unit Instruction Frame0 Unit SceneType Unit
      simpleFrame e = create (start $> scene0 e)

      simpleScene =
        simpleFrame
          @|> loop \fr -> do
            e <- ask
            pure
              $ void
              $ change
              $ fr
                $>
                  { sinOsc: 440.0 + e.time * 50.0
                  , highpass: 330.0 + e.time * 10.0
                  }

      (frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Instr /\ _ /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (_ /\ _ /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (_ /\ _ /\ _) = oneFrame' frame4 { time: 0.4 }

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (pure 1.0)
        , MakeHighpass "highpass" (pure 330.0) (pure 1.0)
        , MakeSinOsc "sinOsc" (pure On) (pure 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
        ]
    it "is coherent after frame0Instr" do
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "is coherent after frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency "highpass" $ pure 331.0, SetFrequency "sinOsc" $ pure 445.0 ]
    it "is coherent after frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency "highpass" $ pure 332.0, SetFrequency "sinOsc" $ pure 450.0 ]
  describe "a scene that forks at 0.3 seconds" do
    let
      simpleFrame :: Time -> WAG Unit Instruction Frame0 Unit SceneType Unit
      simpleFrame e = create (start $> scene0 e)

      simpleScene =
        simpleFrame
          @|> branch \fr -> do
            { time } <- ask
            pure
              $
                if time < 0.3 then
                  Right $ change $ fr $> { highpass: 330.0 + time * 10.0 }
                else
                  Left
                    ( fr
                        # loop \fr' e ->
                          change
                            $ fr'
                              $>
                                { highpass: 330.0 + e.time * 50.0
                                }
                    )

      (frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Instr /\ _ /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Instr /\ _ /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Instr /\ _ /\ _) = oneFrame' frame4 { time: 0.4 }

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (pure 1.0)
        , MakeHighpass "highpass" (pure 330.0) (pure 1.0)
        , MakeSinOsc "sinOsc" (pure On) (pure 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
        ]
    it "branches at frame0Instr" do
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "branches at frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency "highpass" $ pure 331.0 ]
    it "branches at frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency "highpass" $ pure 332.0 ]
    it "branches at frame3Instr" do
      resolveInstructions frame3Instr `shouldEqual` [ SetFrequency "highpass" $ pure 345.0 ]
    it "branches at frame4Instr" do
      resolveInstructions frame4Instr `shouldEqual` [ SetFrequency "highpass" $ pure 350.0 ]
  describe "a scene that forks at 0.3 seconds with ibranch" do
    let
      simpleFrame :: Time -> WAG Unit Instruction Frame0 Unit SceneType Unit
      simpleFrame e = create (start $> scene0 e)

      simpleScene =
        simpleFrame
          @|> ibranch \{ time } _ ->
            if time < 0.3 then
              Right $ ichange { highpass: 330.0 + time * 10.0 }
            else
              Left
                $ icont
                  (iloop \e -> const $ ichange { highpass: { freq: 330.0 + e.time * 50.0, q: e.time } })
                  (ipure unit)

      (frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene { time: 0.0 }

      (frame1Instr /\ _ /\ frame2) = oneFrame' frame1 { time: 0.1 }

      (frame2Instr /\ _ /\ frame3) = oneFrame' frame2 { time: 0.2 }

      (frame3Instr /\ _ /\ frame4) = oneFrame' frame3 { time: 0.3 }

      (frame4Instr /\ _ /\ _) = oneFrame' frame4 { time: 0.4 }

      instructionAssertion =
        [ MakeSpeaker
        , MakeGain "mix" (pure 1.0)
        , MakeHighpass "highpass" (pure 330.0) (pure 1.0)
        , MakeSinOsc "sinOsc" (pure On) (pure 440.0)
        , ConnectXToY "mix" "speaker"
        , ConnectXToY "highpass" "mix"
        , ConnectXToY "mix" "mix"
        , ConnectXToY "sinOsc" "highpass"
        ]
    it "branches at frame0Instr" do
      resolveInstructions frame0Instr `shouldEqual` instructionAssertion
    it "branches at frame1Instr" do
      resolveInstructions frame1Instr `shouldEqual` [ SetFrequency "highpass" $ pure 331.0 ]
    it "branches at frame2Instr" do
      resolveInstructions frame2Instr `shouldEqual` [ SetFrequency "highpass" $ pure 332.0 ]
    it "branches at frame3Instr" do
      resolveInstructions frame3Instr `shouldEqual` [ SetFrequency "highpass" $ pure 345.0, SetQ "highpass" $ pure 0.3 ]
    it "branches at frame4Instr" do
      resolveInstructions frame4Instr `shouldEqual` [ SetFrequency "highpass" $ pure 350.0, SetQ "highpass" $ pure 0.4 ]
