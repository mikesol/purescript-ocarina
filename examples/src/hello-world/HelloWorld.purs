module Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Ocarina.Control (gain_, sinOsc)
import Ocarina.Core (bangOn)
import Ocarina.Run (run2_)

data AudioState = Stopped | Playing { stopPlaying :: Effect Unit }

main :: Effect Unit
main = runInBody Deku.do
  setAudioState /\ audioState <- useState Stopped
  D.button
    [ DL.runOn DL.click $ audioState <#> case _ of
        Playing { stopPlaying } -> do
          stopPlaying
          setAudioState Stopped
        Stopped -> do
          stopPlaying <- run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
          setAudioState (Playing { stopPlaying })
    ]
    [ text
        ( audioState <#> case _ of
            Stopped -> "Start"
            Playing _ -> "Stop"
        )
    ]