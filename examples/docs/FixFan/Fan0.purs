module WAGS.Example.Docs.FixFan.Fan0 where

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

px = Proxy :: Proxy """<div>
  <pre><code>run2_
  $ fan (loopBuf buf pureOn)
      \b -> gain_ 0.8
        ( bandpass_ { frequency: 400.0, q: 1.0 } (input b)
        ~ bandpass_ { frequency: 880.0, q: 5.0 } (input b)
        ~ bandpass_ { frequency: 1200.0, q: 10.0 } (input b)
        ~ bandpass_ { frequency: 2000.0, q: 20.0 } (input b)
        ~ bandpass_ { frequency: 3000.0, q: 30.0 } (input b)
        )</code></pre>

  @ai0@
  </div>
"""

fan0 :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
fan0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0: nut
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