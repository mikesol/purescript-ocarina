module Test.Patch where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Control.Functions (freeze, start, (@||>))
import WAGS.Control.Types (Frame0, WAG, oneFrame')
import WAGS.Graph.AudioUnit (THighpass, TLowpass, TSawtoothOsc, TSinOsc, TSpeaker, _off)
import WAGS.Patch (PatchInfo, patch)
import WAGS.Rendered (Instruction)
import WAGS.Rendered as R

testPatch :: Spec Unit
testPatch = do
  describe "patch" do
    it "renders a simple graph" do
      let
        simpleFrame =
          ( patch
              :: PatchInfo
              -> WAG Unit Instruction Frame0 Unit () Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleScene = simpleFrame start @||> freeze

        (frame0Instr /\ _ /\ _) = oneFrame' simpleScene unit
      (map ((#) unit) frame0Instr) `shouldEqual`
        [ R.iMakeSpeaker
        , R.iMakeSinOsc { id: "sinOsc", onOff: (pure _off), freq: (pure 440.0) }
        , R.iConnectXToY { fromId: "sinOsc", fromUnit: "PATCH", toId: "speaker", toUnit: "PATCH" }
        ]
    it "makes a no op a no op" do
      let
        startingFrame =
          ( patch
              :: PatchInfo
              -> WAG Unit Instruction Frame0 Unit () Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleFrame =
          ( patch
              :: forall proof
               . PatchInfo
              -> WAG Unit Instruction proof Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
              -> WAG Unit Instruction proof Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleScene =
          startingFrame start
            @||>
              ( \fr ->
                  (simpleFrame fr)
                    @||> freeze
              )

        (_ /\ _ /\ frame1) = oneFrame' simpleScene unit

        (frame1Instr /\ _ /\ _) = oneFrame' frame1 unit
      (map ((#) unit) frame1Instr) `shouldEqual` []
    it "removes a single connection" do
      let
        startingFrame =
          ( patch
              :: PatchInfo
              -> WAG Unit Instruction Frame0 Unit () Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleFrame =
          ( patch
              :: forall proof
               . PatchInfo
              -> WAG Unit Instruction proof Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
              -> WAG Unit Instruction proof Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleScene =
          startingFrame start
            @||>
              ( \fr ->
                  (simpleFrame fr)
                    @||> freeze
              )

        (_ /\ _ /\ frame1) = oneFrame' simpleScene unit

        (frame1Instr /\ _ /\ _) = oneFrame' frame1 unit
      (map ((#) unit) frame1Instr) `shouldEqual` [ R.iDisconnectXFromY { fromId: "sinOsc", fromUnit: "PATCH", toId: "speaker", toUnit: "PATCH" } ]
    it "correctly handles complex graph" do
      let
        startingFrame =
          ( patch
              :: PatchInfo
              -> WAG Unit Instruction Frame0 Unit
                   ()
                   Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleFrame =
          ( patch
              :: forall proof
               . PatchInfo
              -> WAG Unit Instruction proof Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
              -> WAG Unit Instruction proof Unit
                   ( speaker :: TSpeaker /\ { anotherOsc :: Unit }
                   , anotherOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleScene =
          startingFrame start
            @||>
              ( \fr ->
                  (simpleFrame fr)
                    @||> freeze
              )

        (_ /\ _ /\ frame1) = oneFrame' simpleScene unit

        (frame1Instr /\ _ /\ _) = oneFrame' frame1 unit
      (map ((#) unit) frame1Instr) `shouldEqual`
        [ R.iDisconnectXFromY { fromId: "sinOsc", fromUnit: "PATCH", toId: "highpass", toUnit: "PATCH" }
        , R.iDisconnectXFromY { fromId: "highpass", fromUnit: "PATCH", toId: "speaker", toUnit: "PATCH" }
        , R.iDisconnectXFromY { fromId: "sinOsc", fromUnit: "PATCH", toId: "speaker", toUnit: "PATCH" }
        , R.iDestroyUnit { id: "highpass", unit: "PATCH" }
        , R.iDestroyUnit { id: "sinOsc", unit: "PATCH" }
        , R.iMakeSinOsc { id: "anotherOsc", onOff: (pure _off), freq: (pure 440.0) }
        , R.iConnectXToY { fromId: "anotherOsc", fromUnit: "PATCH", toId: "speaker", toUnit: "PATCH" }
        ]
    it "leaves noop in complex graph" do
      let
        startingFrame =
          ( patch
              :: PatchInfo
              -> WAG Unit Instruction Frame0 Unit
                   ()
                   Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit, lowpass :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   , lowpass :: TLowpass /\ { sawtoothOsc :: Unit }
                   , sawtoothOsc :: TSawtoothOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleFrame =
          ( patch
              :: forall proof
               . PatchInfo
              -> WAG Unit Instruction proof Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit, lowpass :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   , lowpass :: TLowpass /\ { sawtoothOsc :: Unit }
                   , sawtoothOsc :: TSawtoothOsc /\ {}
                   )
                   Unit
              -> WAG Unit Instruction proof Unit
                   ( speaker :: TSpeaker /\ { anotherOsc :: Unit, lowpass :: Unit }
                   , anotherOsc :: TSinOsc /\ {}
                   , lowpass :: TLowpass /\ { sawtoothOsc :: Unit }
                   , sawtoothOsc :: TSawtoothOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing, mediaElement: Nothing }

        simpleScene =
          startingFrame start
            @||>
              ( \fr ->
                  (simpleFrame fr)
                    @||> freeze
              )

        (_ /\ _ /\ frame1) = oneFrame' simpleScene unit

        (frame1Instr /\ _ /\ _) = oneFrame' frame1 unit
      (map ((#) unit) frame1Instr) `shouldEqual`
        [ R.iDisconnectXFromY { fromId: "sinOsc", fromUnit: "PATCH", toId: "highpass", toUnit: "PATCH" }
        , R.iDisconnectXFromY { fromId: "highpass", fromUnit: "PATCH", toId: "speaker", toUnit: "PATCH" }
        , R.iDisconnectXFromY { fromId: "sinOsc", fromUnit: "PATCH", toId: "speaker", toUnit: "PATCH" }
        , R.iDestroyUnit { id: "highpass", unit: "PATCH" }
        , R.iDestroyUnit { id: "sinOsc", unit: "PATCH" }
        , R.iMakeSinOsc { id: "anotherOsc", onOff: (pure _off), freq: (pure 440.0) }
        , R.iConnectXToY { fromId: "anotherOsc", fromUnit: "PATCH", toId: "speaker", toUnit: "PATCH" }
        ]
