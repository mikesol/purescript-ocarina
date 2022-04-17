module WAGS.Example.Docs.AudioUnits.Bandpass where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (bandpass_, loopBuf, gain_, (~))
import WAGS.Core (fan, input)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px = Proxy :: Proxy """<section>
  <h2 id="bandpass">Bandpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">bandpass filter</a> isolates a single frequency range of a source. When you crank up a bandpass node's Q value, the isolation gets more intense. At the extreme, the source signal is almost lost and you get a pure sound that resembles a sine-wave oscillator.</p>

  <pre><code>run2_
  $ fan (loopBuf buf pureOn)
      \b -> gain_ 0.8
        ( bandpass_ { frequency: 400.0, q: 1.0 } (input b)
        ~ bandpass_ { frequency: 880.0, q: 5.0 } (input b)
        ~ bandpass_ { frequency: 1200.0, q: 10.0 } (input b)
        ~ bandpass_ { frequency: 2000.0, q: 20.0 } (input b)
        ~ bandpass_ { frequency: 3000.0, q: 30.0 } (input b)
        )</code></pre>

  @bandpass@
  </section>
"""

bandpass :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
bandpass ccb _ ev = makePursx' (Proxy :: _ "@") px
  { bandpass: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \buf -> run2_
            $ fan (loopBuf buf pureOn)
                \b -> gain_ 0.8
                  ( bandpass_ { frequency: 400.0, q: 1.0 } (input b)
                  ~ bandpass_ { frequency: 880.0, q: 5.0 } (input b)
                  ~ bandpass_ { frequency: 1200.0, q: 10.0 } (input b)
                  ~ bandpass_ { frequency: 2000.0, q: 20.0 } (input b)
                  ~ bandpass_ { frequency: 3000.0, q: 30.0 } (input b)
                  )

      )
  }

  {-
fan (loopBuf buf pureOn)
  \b -> gain_ 0.8
    ( bandpass_ { frequency: 400.0, q: 1.0 } (input b)
    ~ bandpass_ { frequency: 880.0, q: 5.0 } (input b)
    ~ bandpass_ { frequency: 1200.0, q: 10.0 } (input b)
    ~ bandpass_ { frequency: 2000.0, q: 20.0 } (input b)
    ! bandpass_ { frequency: 3000.0, q: 30.0 } (input b)
    )


  -}