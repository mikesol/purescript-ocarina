module WAGS.Example.Docs.AudioUnits.LoopBuf where

import Prelude

import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Parameter (bangOn)
import WAGS.Run (run2_)

px =
  Proxy
    :: Proxy
         """<section>
  <h2 id="loopbuf">Looping buffer</h2>

  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">looping buffer</a> is buffered audio that loops. The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start and end and optionally its duration.</p>

  <pre><code>\buf -> run2_
  [ loopBuf
      { buffer: buf
      , playbackRate: 0.5
      , loopStart: 0.1
      , loopEnd: 0.6
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.0
      , loopStart: 0.5
      , loopEnd: 1.2
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.7
      }
      bangOn
  ]</code></pre>

  @loopBuf@
  </section>
"""

loopBufEx
  :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
loopBufEx ccb _ ev = makePursx' (Proxy :: _ "@") px
  { loopBuf: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/100/100981_1234256-lq.mp3")
          \buf -> run2_
            [ loopBuf
                { buffer: buf
                , playbackRate: 0.5
                , loopStart: 0.1
                , loopEnd: 0.6
                }
                bangOn
            , loopBuf
                { buffer: buf
                , playbackRate: 1.0
                , loopStart: 0.5
                , loopEnd: 1.2
                }
                bangOn
            , loopBuf
                { buffer: buf
                , playbackRate: 1.7
                }
                bangOn
            ]
      )
  }