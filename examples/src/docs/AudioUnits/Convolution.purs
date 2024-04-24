module Ocarina.Example.Docs.AudioUnits.Convolution where

import Prelude


import Deku.Core (Nut)
import Deku.Pursx (pursx)
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (convolver, loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri)
import Ocarina.Run (run2)

type Px = """<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
"""

convolution :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
convolution ccb _ ev =pursx @Px
  { convolution:
      (  audioWrapper ev ccb (\ctx -> { loop: _, verb: _ }
         <$> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3" <*> decodeAudioDataFromUri ctx "https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a")
          \ctx {loop, verb} -> run2 ctx
            [convolver verb [loopBuf loop bangOn]]
      )
  }