module WAGS.CheatSheet.Feedback where
import WAGS.Create.Optionals as W

graph time = W.speaker {
  vocals: W.gain 0.5 { microphone: W.microphone, delay: W.delay 0.1 { quiet: W.gain 0.4 { vocals: W.ref } } }
}