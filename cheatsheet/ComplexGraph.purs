module WAGS.CheatSheet.ComplexGraph where

import Prelude

import Math (sin, cos, pi)
import Type.Proxy (Proxy(..))
import WAGS.Create.Optionals as W

graph time myBuf = W.speaker {
  mainBus: W.gain 1.0 {
     vocals: W.gain 0.5 W.microphone_,
     loop0: W.gain 0.2 (W.loopBuf { loopStart: 0.1, loopEnd: 0.5 } myBuf),
     loop1: W.gain 0.5 (W.highpass 2000.0 (W.loopBuf { loopStart: 0.2, loopEnd: 1.5 } myBuf)),
     pad: W.gain 0.12 {
       osc0: W.sinOsc $ 220.0 + sin (pi * time * 3.0) * 10.0,
       osc1: W.sinOsc $ 350.0 + cos (pi * time * 1.3) * 20.0,
       osc2: W.sinOsc $ 490.0 - sin (pi * time * 0.5) * 50.0,
       osc3: W.sinOsc $ 510.0 - cos (pi * time * 20.0) * 100.0,
       osc4: W.gain 0.1 { descant: W.sinOsc 2050.0 }
     }
  }
}