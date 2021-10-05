module Test.Patch where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Control.Functions (freeze, start, (@||>))
import WAGS.Control.Types (Frame0, WAG, oneFrame')
import WAGS.Graph.AudioUnit (OnOff(..), THighpass, TSinOsc, TSpeaker, TLowpass, TSawtoothOsc)
import WAGS.Patch (patch)
import WAGS.Rendered (Instruction(..))
import WAGS.WebAPI (BrowserMicrophone)

testPatch :: Spec Unit
testPatch = do
  describe "patch" do
    it "renders a simple graph" do
      let
        simpleFrame =
          ( patch
              :: { microphone :: Maybe BrowserMicrophone }
              -> WAG Unit Instruction Frame0 Unit () Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing }

        simpleScene = simpleFrame start @||> freeze

        (frame0Instr /\ _ /\ _) = oneFrame' simpleScene unit
      (map ((#) unit) frame0Instr) `shouldEqual`
        [ MakeSpeaker
        , MakeSinOsc "sinOsc" (pure Off) (pure 440.0)
        , ConnectXToY "sinOsc" "PATCH" "speaker" "PATCH"
        ]
    it "makes a no op a no op" do
      let
        startingFrame =
          ( patch
              :: { microphone :: Maybe BrowserMicrophone }
              -> WAG Unit Instruction Frame0 Unit () Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing }

        simpleFrame =
          ( patch
              :: forall proof
               . { microphone :: Maybe BrowserMicrophone }
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
          ) { microphone: Nothing }

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
              :: { microphone :: Maybe BrowserMicrophone }
              -> WAG Unit Instruction Frame0 Unit () Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing }

        simpleFrame =
          ( patch
              :: forall proof
               . { microphone :: Maybe BrowserMicrophone }
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
          ) { microphone: Nothing }

        simpleScene =
          startingFrame start
            @||>
              ( \fr ->
                  (simpleFrame fr)
                    @||> freeze
              )

        (_ /\ _ /\ frame1) = oneFrame' simpleScene unit

        (frame1Instr /\ _ /\ _) = oneFrame' frame1 unit
      (map ((#) unit) frame1Instr) `shouldEqual` [ DisconnectXFromY "sinOsc" "PATCH" "speaker" "PATCH" ]
    it "correctly handles complex graph" do
      let
        startingFrame =
          ( patch
              :: { microphone :: Maybe BrowserMicrophone }
              -> WAG Unit Instruction Frame0 Unit
                   ()
                   Unit
              -> WAG Unit Instruction Frame0 Unit
                   ( speaker :: TSpeaker /\ { highpass :: Unit, sinOsc :: Unit }
                   , highpass :: THighpass /\ { sinOsc :: Unit }
                   , sinOsc :: TSinOsc /\ {}
                   )
                   Unit
          ) { microphone: Nothing }

        simpleFrame =
          ( patch
              :: forall proof
               . { microphone :: Maybe BrowserMicrophone }
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
          ) { microphone: Nothing }

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
        [ (DisconnectXFromY "sinOsc" "PATCH" "highpass" "PATCH")
        , (DisconnectXFromY "highpass" "PATCH" "speaker" "PATCH")
        , (DisconnectXFromY "sinOsc" "PATCH" "speaker" "PATCH")
        , (DestroyUnit "highpass" "PATCH")
        , (DestroyUnit "sinOsc" "PATCH")
        , (MakeSinOsc "anotherOsc" (pure Off) (pure 440.0))
        , (ConnectXToY "anotherOsc" "PATCH" "speaker" "PATCH")
        ]
    it "leaves noop in complex graph" do
      let
        startingFrame =
          ( patch
              :: { microphone :: Maybe BrowserMicrophone }
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
          ) { microphone: Nothing }

        simpleFrame =
          ( patch
              :: forall proof
               . { microphone :: Maybe BrowserMicrophone }
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
          ) { microphone: Nothing }

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
        [ (DisconnectXFromY "sinOsc" "PATCH" "highpass" "PATCH")
        , (DisconnectXFromY "highpass" "PATCH" "speaker" "PATCH")
        , (DisconnectXFromY "sinOsc" "PATCH" "speaker" "PATCH")
        , (DestroyUnit "highpass" "PATCH")
        , (DestroyUnit "sinOsc" "PATCH")
        , (MakeSinOsc "anotherOsc" (pure Off) (pure 440.0))
        , (ConnectXToY "anotherOsc" "PATCH" "speaker" "PATCH")
        ]
