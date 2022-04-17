module WAGS.Example.Docs.FixFan.Fix0 where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (bandpass_, delay_, gain_, loopBuf, playBuf, (~))
import WAGS.Core (fan, fix, input)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px = Proxy :: Proxy """<div>
  <pre><code>run2_
  $ fix
      \b -> gain_ 1.0
        ( playBuf buf pureOn
        ~ delay_ 0.1 (gain_ 0.6 (input b))
        )</code></pre>

  @ai0@
  </div>
"""

fix0 :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
fix0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ai0: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
          \buf -> run2_
            $ fix
                \b -> gain_ 1.0
                  ( playBuf buf pureOn
                  ~ delay_ 0.1 (gain_ 0.6 (input b))
                  )
      )
  }
