module WAGS.CheatSheet.SimpleGraph where

import WAGS.Create.Optionals as W

graph = W.speaker { myGain: W.gain 0.1 { myOsc: W.sinOsc 440.0 } }