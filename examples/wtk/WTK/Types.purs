module WAGS.Example.WTK.Types where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Set as S
import Data.Tuple.Nested (type (/\))
import FRP.Event.MIDI (MIDIEventInTime)
import WAGS.Create.Optionals (CGain, CSinOsc, CSpeaker, gain, sinOsc, speaker)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TSinOsc, TSpeaker)

data Key
  = K0
  | K1
  | K2
  | K3
  | K4
  | K5
  | K6
  | K7
  | K8
  | K9

type KlavierTemplate
  = CSpeaker
      { mix ::
          CGain
            { k0 :: CGain { osc0 :: CSinOsc }
            , k1 :: CGain { osc1 :: CSinOsc }
            , k2 :: CGain { osc2 :: CSinOsc }
            , k3 :: CGain { osc3 :: CSinOsc }
            , k4 :: CGain { osc4 :: CSinOsc }
            , k5 :: CGain { osc5 :: CSinOsc }
            , k6 :: CGain { osc6 :: CSinOsc }
            , k7 :: CGain { osc7 :: CSinOsc }
            , k8 :: CGain { osc8 :: CSinOsc }
            , k9 :: CGain { osc9 :: CSinOsc }
            }
      }

type KlavierType
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { k0 :: Unit, k1 :: Unit, k2 :: Unit, k3 :: Unit, k4 :: Unit, k5 :: Unit, k6 :: Unit, k7 :: Unit, k8 :: Unit, k9 :: Unit }
    , k0 :: TGain /\ { osc0 :: Unit }
    , osc0 :: TSinOsc /\ {}
    , k1 :: TGain /\ { osc1 :: Unit }
    , osc1 :: TSinOsc /\ {}
    , k2 :: TGain /\ { osc2 :: Unit }
    , osc2 :: TSinOsc /\ {}
    , k3 :: TGain /\ { osc3 :: Unit }
    , osc3 :: TSinOsc /\ {}
    , k4 :: TGain /\ { osc4 :: Unit }
    , osc4 :: TSinOsc /\ {}
    , k5 :: TGain /\ { osc5 :: Unit }
    , osc5 :: TSinOsc /\ {}
    , k6 :: TGain /\ { osc6 :: Unit }
    , osc6 :: TSinOsc /\ {}
    , k7 :: TGain /\ { osc7 :: Unit }
    , osc7 :: TSinOsc /\ {}
    , k8 :: TGain /\ { osc8 :: Unit }
    , osc8 :: TSinOsc /\ {}
    , k9 :: TGain /\ { osc9 :: Unit }
    , osc9 :: TSinOsc /\ {}
    }

fullKeyboard :: KlavierTemplate
fullKeyboard =
  speaker
    { mix:
        gain 1.0
          { k0: gain 0.0 { osc0: sinOsc { onOff: Off, freq: 440.0 } }
          , k1: gain 0.0 { osc1: sinOsc { onOff: Off, freq: 440.0 } }
          , k2: gain 0.0 { osc2: sinOsc { onOff: Off, freq: 440.0 } }
          , k3: gain 0.0 { osc3: sinOsc { onOff: Off, freq: 440.0 } }
          , k4: gain 0.0 { osc4: sinOsc { onOff: Off, freq: 440.0 } }
          , k5: gain 0.0 { osc5: sinOsc { onOff: Off, freq: 440.0 } }
          , k6: gain 0.0 { osc6: sinOsc { onOff: Off, freq: 440.0 } }
          , k7: gain 0.0 { osc7: sinOsc { onOff: Off, freq: 440.0 } }
          , k8: gain 0.0 { osc8: sinOsc { onOff: Off, freq: 440.0 } }
          , k9: gain 0.0 { osc9: sinOsc { onOff: Off, freq: 440.0 } }
          }
    }

type Trigger
  = List { time :: Instant, value :: MIDIEventInTime }

type KeyUnit
  = { gain :: Number, freq :: Number, onOff :: OnOff }

type KeyInfo
  = { startU :: KeyUnit
    , endU :: KeyUnit
    , sustainU :: Number -> KeyUnit
    , startT :: Number
    , cps :: Number
    , keyDuration :: Number
    , i :: Int
    , k :: Key
    }

type MakeRenderingEnv
  = Maybe Trigger ->
    Number ->
    List Key ->
    List KeyInfo ->
    { onsets :: List KeyInfo
    , newCurrentKeys :: List KeyInfo
    , notesOff :: S.Set Int
    , newAvailableKeys :: List Key
    , futureCurrentKeys :: List KeyInfo
    , futureAvailableKeys :: List Key
    }
