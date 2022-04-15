module WAGS.Example.Docs.AudioUnits.PlayBuf where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (playBuf)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px =  Proxy :: Proxy   """<section>
  <h2 id="playbuf">Playing a buffer</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">Playback from a buffer</a> is one of the bread-and-butter operations in Web Audio (or any audio). The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start time and optionally its duration.</p>

  <pre><code>\buf -> run2_
  [ playBuf
      { buffer
      , duration: 3.0
      , bufferOffset: 4.2
      }
      pureOn
  ]
</code></pre>

  ~playBuf~
  </section>
"""

playBufEx
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
playBufEx ccb _ ev = px ~~
  { playBuf: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/470/470035_9564355-lq.mp3")
          \buffer -> run2_
            [ playBuf
                { buffer
                , duration: 3.0
                , bufferOffset: 4.2
                }
                pureOn
            ]
      )
  }