module Test.Patch where

import Prelude
import Data.Map as M
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Functions ((@|>), freeze)
import WAGS.Control.Types (Frame, Frame0, oneFrame')
import WAGS.Graph.AudioUnit (OnOff(..), THighpass, TSinOsc, TSpeaker)
import WAGS.Graph.Parameter (param)
import WAGS.Patch (patch)
import WAGS.Rendered (AnAudioUnit(..), Instruction(..))

testPatch :: Spec Unit
testPatch = do
  describe "patch" do
    it "renders a simple graph" do
      let
        simpleFrame =
          ( patch ::
              Frame Unit Unit Instruction Frame0 {}
                { speaker :: TSpeaker /\ { sinOsc :: Unit }
                , sinOsc :: TSinOsc /\ {}
                }
                Unit
          )

        simpleScene = simpleFrame @|> freeze

        (frame0Nodes /\ frame0Edges /\ frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene unit
      frame0Nodes `shouldEqual` (M.fromFoldable [ "sinOsc" /\ ASinOsc Off (param 440.0), "speaker" /\ ASpeaker ])
      frame0Edges `shouldEqual` (M.fromFoldable [ "speaker" /\ S.fromFoldable [ "sinOsc" ] ])
      (map ((#) unit) frame0Instr) `shouldEqual` [ MakeSinOsc "sinOsc" Off (param 440.0), MakeSpeaker, ConnectXToY "sinOsc" "speaker" ]
    it "makes a no op a no op" do
      let
        startingFrame =
          ( patch ::
              Frame Unit Unit Instruction Frame0
                {}
                { speaker :: TSpeaker /\ { highpass :: Unit }
                , highpass :: THighpass /\ { sinOsc :: Unit }
                , sinOsc :: TSinOsc /\ {}
                }
                Unit
          )

        simpleFrame =
          ( patch ::
              forall proof.
              Frame Unit Unit Instruction proof
                { speaker :: TSpeaker /\ { highpass :: Unit }
                , highpass :: THighpass /\ { sinOsc :: Unit }
                , sinOsc :: TSinOsc /\ {}
                }
                { speaker :: TSpeaker /\ { highpass :: Unit }
                , highpass :: THighpass /\ { sinOsc :: Unit }
                , sinOsc :: TSinOsc /\ {}
                }
                Unit
          )

        simpleScene =
          startingFrame
            @|> ( \fr ->
                  ( WAGS.do
                      fr
                      simpleFrame
                  )
                    @|> freeze
              )

        (frame0Nodes /\ frame0Edges /\ frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene unit

        (frame1Nodes /\ frame1Edges /\ frame1Instr /\ _ /\ frame2) = oneFrame' frame1 unit
      --frame1Nodes `shouldEqual` (M.fromFoldable [])
      --frame1Edges `shouldEqual` (M.fromFoldable [])
      (map ((#) unit) frame1Instr) `shouldEqual` []
    it "removes a single connection" do
      let
        startingFrame =
          ( patch ::
              Frame Unit Unit Instruction Frame0
                {}
                { speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit }
                , highpass :: THighpass /\ { sinOsc :: Unit }
                , sinOsc :: TSinOsc /\ {}
                }
                Unit
          )

        simpleFrame =
          ( patch ::
              forall proof.
              Frame Unit Unit Instruction proof
                { speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit }
                , highpass :: THighpass /\ { sinOsc :: Unit }
                , sinOsc :: TSinOsc /\ {}
                }
                { speaker :: TSpeaker /\ { highpass :: Unit }
                , highpass :: THighpass /\ { sinOsc :: Unit }
                , sinOsc :: TSinOsc /\ {}
                }
                Unit
          )

        simpleScene =
          startingFrame
            @|> ( \fr ->
                  ( WAGS.do
                      fr
                      simpleFrame
                  )
                    @|> freeze
              )

        (frame0Nodes /\ frame0Edges /\ frame0Instr /\ _ /\ frame1) = oneFrame' simpleScene unit

        (frame1Nodes /\ frame1Edges /\ frame1Instr /\ _ /\ frame2) = oneFrame' frame1 unit
      --frame0Nodes `shouldEqual` (M.fromFoldable [])
      --frame0Edges `shouldEqual` (M.fromFoldable [])
      (map ((#) unit) frame1Instr) `shouldEqual` [ DisconnectXFromY "sinOsc" "speaker" ]
